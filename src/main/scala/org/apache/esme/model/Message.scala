package org.apache.esme.model

/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import net.liftweb._
import mapper._
import util._
import textile.TextileParser._
import common._
import http._
import js._
import Helpers._

import scala.xml._
import scala.xml.transform.{RuleTransformer, RewriteRule}

import java.util.regex.Matcher

import org.compass.annotations._
import bootstrap.liftweb.Compass.compass
import org.compass.core._
import lucene.util._
import org.apache.lucene.index.TermFreqVector
import org.tartarus.snowball.ext.PorterStemmer

import org.apache.esme._
import lib._

object Message extends Message with LongKeyedMetaMapper[Message] with Logger {
  val logger: Logger = Logger ("org.apache.esme.model.Message")
                                      
  private def fixConversation(msg: Message) {
    if (!msg.conversation.defined_? && msg.replyTo.defined_?) {
      for (replyTo <- msg.replyTo.obj) {
        if (!replyTo.conversation.defined_?) {
          replyTo.conversation(replyTo.id).save
        }
        msg.conversation(replyTo.conversation)
      }
    }
  }      
  
  val root = LiftRules.context.path

  def cacheSize: Int = 10000

  private val idCache = new LRU[Long, Message](cacheSize)

  private val stemmer:PorterStemmer = new PorterStemmer()

  /**
   * A list of messages is requested from a simple cache
   * The cache is filtered for messages which the user has permission to view
   */
  def findMessages(in: Seq[Long]): Map[Long, Message] = synchronized {
    val il = in.toList
    val user = User.currentUser
    val (r1, left) = il.foldLeft[(Map[Long, Message], List[Long])](
      (Map.empty, Nil)) {
      case ((map, left), id) =>
        if (idCache.contains(id)) {
          if (!user.isDefined || 
              !idCache(id).pool.defined_? ||
              Privilege.findViewablePools(user.get.id.is).
                        contains(idCache(id).pool.is)) {
            (map + (id -> idCache(id)), left)
          } else
            (map, left)
        } else (map, id :: left)
    }


    val r2 = left match {
      case Nil => r1
      case xs =>
        findAndPrime(InRaw(id, xs.mkString(","),
                           IHaveValidatedThisSQL("dpp", "Aug 25, 2008"))).
        foldLeft(r1) {
          case (map, msg) =>
            idCache(msg.id) = msg
            map + (msg.id.is -> msg)
        }
    }

    r2
  }

  private def uncache(msg: Message): Unit = synchronized {
    idCache.remove(msg.id)
  }

  def indexMessage(msg: Message) {
    for(session <- compass.map(_.openSession())) yield {
      var tx:CompassTransaction = null

      try {
        tx = session.beginTransaction()
        session.save(msg)
        tx.commit();
      } catch  {
        case ce: CompassException =>
          if (tx != null) tx.rollback();
      } finally {
        session.close();
      }
    }
  }

  override def afterCommit = indexMessage _ :: super.afterCommit

  private def saveTags(msg: Message) {
    msg.saveTheTags()
  }

  override def afterCreate = saveTags _ :: super.afterCreate
  
  override def beforeCreate = fixConversation _ :: super.beforeCreate

  override def afterSave = uncache _ :: super.afterSave

  def findAndPrime(params: QueryParam[Message]*): List[Message] = {
    val ret: List[Message] = this.findAll(params :_*)


    val userIds: List[Long] = (ret.flatMap(_.author.box) :::
                               ret.flatMap(_.sentToIds)).distinct

    val users:Map[Long, User] = Map(User.findAll(InRaw(User.id, userIds.mkString(","),
                                                       IHaveValidatedThisSQL("dpp", "Aug 23, 2008"))).map(u => (u.id.is, u)) :_*)

    ret.foreach(_.preload(users))
    ret
  }

  def search(searchTerm: String, following: List[User], numHits: Int): List[Message] = {
    val users:List[String] = following.map(user => user.nickname.is)

    logger.debug("Inside Message.search() with user list "+(users.mkString(", ")))

    (for(session <- compass.map(_.openSession()); user <- User.currentUser) yield {
        var tx:CompassTransaction = null
        var returnValue:List[Message] = Nil

        try {
          tx = session.beginTransaction()
          val queryBuilder: CompassQueryBuilder = session.queryBuilder()

          val followingQuery = queryBuilder.bool().addShould(queryBuilder.term("author", user.id))
          for (user <- following) followingQuery.addShould(queryBuilder.term("author", user.id))

          val query: CompassQuery = queryBuilder.bool()
          .addMust( queryBuilder.term("text", stemWord(searchTerm.toLowerCase() )) )
          .addMust( followingQuery.toQuery )
          .toQuery()

          logger.debug("query is "+query.toString)

          val hitlist = query
          .addSort("when", CompassQuery.SortPropertyType.STRING, CompassQuery.SortDirection.REVERSE)
          .hits().detach(0, numHits)

          logger.debug("Detached hits: "+hitlist.totalLength)

          val resourceList = hitlist.getResources.toList.asInstanceOf[List[Resource]]

          val msgIds = resourceList.map(_.getId.toLong)
          returnValue = Message.findMessages(msgIds).values.toList
          tx.commit();
        } catch  {
          case ce: CompassException =>
            if (tx != null) tx.rollback();
        } finally {
          session.close();
        }

        returnValue
      }) openOr Nil
  }
  
  /**
   * Lift's findMapDb is overridden to ensure that message search will return
   * only messages which the user has permission to view
   *
   * All Lift finder methods eventually call findMapDb, which makes it suitable
   * to override to redefine some global behavior.
   */
  override def findMapDb[T](dbId : ConnectionIdentifier, by : QueryParam[Message]*)(f : (Message) => Box[T]): List[T] = {
    // modify behavior of find methods so that results include only authorized pools of current user      
    val viewablePools =
      for (user <- User.currentUser) yield {
        Privilege.findViewablePools(user.id.is)
      }                                      
    val newQueryParams: Seq[QueryParam[Message]] = viewablePools match {
      case Full(Nil) => List(By(pool, Empty))
      case Full(pools: List[Long]) => List(
        BySql(" POOL in ( ?" + ( ", ?" * (pools.size - 1)) + " ) OR POOL IS NULL ",
              IHaveValidatedThisSQL("vdichev", "22 June 2009"),
              pools.toSeq:_*)
      )
      case _ => Nil
    }                                           
    val modifiedQueryParams = by ++ newQueryParams
    logger.debug("Modified query: " + modifiedQueryParams)
    super.findMapDb(dbId, modifiedQueryParams:_*)(f)
  }

  // Compounds a bunch of (String, Int) elements so that [(String1, Int1), (String2, Int2)] becomes [(StringN, Int1+Int2)]
  //  if String1 and String2 have the same stem (according to the Porter stemming algorithm). StringN is the shorter of
  //  String1 and String2
  private[model] def compoundStem(llsi: List[(String,Int)]): List[(String,Int)] = {
    val stemCache = llsi.foldLeft[Map[String, String]](Map.empty){
      case (map, (str, _)) => if (map.contains(str)) map
        else map + (str -> stemWord(str))
    }
    def shortWord(a: String, b: String): String =
    if (a.length < b.length) a else b

    val stemToWord: Map[String, String] = Map(
      // create a map from stem to all the words that
      // stem down to that word
      stemCache.toList.
      foldLeft[Map[String, List[String]]](Map.empty){
        case (map, (word, stem)) =>
          map + (stem -> (word :: map.getOrElse(stem, Nil)))
      }.toList.
      // convert the list of stemmed words to the shortest word
      // matching the stem
      map{
        case (key, value) => (key, value.reduceLeft(shortWord))
      } :_*)

    llsi.foldLeft[Map[String, Int]](Map.empty){
      case (map, (str, cnt)) =>
        val sw = stemCache(str)
        map + (sw -> (map.getOrElse(sw, 0) + cnt))
    }.toList.map{ case (stem, cnt) => (stemToWord(stem), cnt)}
  }
  
  def centreWeightedTopNWordFreqs(messages: List[Message], n:Int):List[(String, Float)] = {
    val weights = compoundStem(messages.flatMap(_.wordFrequencies))

    // Start with the top N tags
    val sortedWeights = weights.sortWith(_._2 > _._2).take(n)

    // And create a normalized cente-weighted list, e.g. smallest, small, Larger, BIG, *HUGE*, BIG, Larger, small, smallest
    TagUtils.normalize(TagUtils.everyEven(sortedWeights).reverse ::: TagUtils.everyOdd(sortedWeights))
  }

  /**
   * Stem an incoming string
   */
  def stemWord(in: String): String = stemmer.synchronized {
    stemmer.setCurrent(in)
    stemmer.stem()
    stemmer.getCurrent()
  }

  def transformBody(ns: NodeSeq) = {
    toXml.map(new RuleTransformer(new RewriteRule{
      override def transform(n: Node) = n match {
        case e: Elem if "body" == e.label => <body>{ns}</body>
        case _ => n
      }
    })).head
  }

}


@Searchable
class Message extends LongKeyedMapper[Message] with ManyToMany {
  import Message._
  
  def getSingleton = Message // what's the "meta" server
  def primaryKeyField = id

  object id extends MappedLongIndex(this)

  object author extends MappedLongForeignKey(this, User) {
    override def asJs = 
      List("author" -> (obj.map(_.asJs) openOr JE.JsNull))
  }


  private[model] object text extends MappedText(this){
    override lazy val asJsExp =
      JE.Str(is.replaceFirst("(?<=<body>).*(?=</body>)",
                             Matcher.quoteReplacement(digestedXHTML.mkString)))
  }

  object when extends MappedLong(this) {
    override def defaultValue = millis
  }

  object source extends MappedPoliteString(this, 32) {
    def sourceAttr: Option[NodeSeq] = is match {
      case null | "" => None
      case xs => Some(Text(xs))
     }
   }
 
  lazy val body: String = originalXml \ "body" text
  
  //lazy val metadata: String = originalXml \ "metadata" text
  lazy val metadata: String = (originalXml \ "metadata").toString

  lazy val metaData: String = {
    val org = originalXml

    (org \ "metadata").map(_.text).head
  }


  object replyTo extends MappedLongForeignKey(this, Message)

  object conversation extends MappedLongForeignKey(this, Message)  
  
  object followers extends MappedManyToMany(UserConvFollow, UserConvFollow.conversation, UserConvFollow.user, User) 

  object pool extends MappedLongForeignKey(this, AccessPool) {
    override def asJs = 
      List("pool" -> (obj.map(_.asJs) openOr JE.JsNull))
  }
  
  private[model] def preload(users: Map[Long, User]) {
    author.box.foreach{
      id =>
      this.author.primeObj(users.get(id))
    }

    primeNameMap(users)
  }

  private def replyToTag: MetaData =
  new UnprefixedAttribute("reply_to",
                          replyTo.box.map(i => Text(i.toString)).toOption,
                          Null)

  private def conversationTag: MetaData =
  new UnprefixedAttribute("conversation",
                          conversation.box.map(i => Text(i.toString)).toOption,
                          Null)

  override lazy val toXml: Elem = {
    val org = originalXml

    <message id={id.toString} source={source.sourceAttr} when={when.is.toString}
      date={toInternetDate(when.is)}>
      {
        author.obj.map(u =>
          <author name={u.niceName} id={u.id.toString}
            image={u.image}/>
        ) openOr Text("")
      }
      <body>{
          (org \ "body").map(_.child map {
              case e: Elem if e.label == "at_name" =>
                e.attribute("id").map(id =>
                  <at_name image={nameMap(id.text.toLong).image} id={id}
                    nickname={nameMap(id.text.toLong).nickname.is}/>) getOrElse Text("")
              case x => x
            })
        }</body>{
        org \ "tags"
      }{
        org \ "metadata"
      }</message> % replyToTag % conversationTag

  }

  lazy val msgInfoTransformer = new RuleTransformer(
    new RewriteRule {
      override def transform(n: Node) = n match {
        case e: Elem if e.label == "at_name" =>
          e.attribute("nickname").
          map(nickname =>
            <xml:group> @<a href={root+"/user/"+urlEncode(nickname.text)}>{nickname}</a> </xml:group>).
          getOrElse(Text(""))

        case e: Elem if e.label == "tag" =>
          e.attribute("name").map(tag =>
            <xml:group> #<a href={root+"/tag/"+urlEncode(tag.text)}>{tag}</a> </xml:group>).
          getOrElse(Text(""))

        case e: Elem if e.label == "url" =>
          e.attribute("url").flatMap(url =>
            e.attribute("name").flatMap(name =>
              e.attribute("uniqueId").map { id =>
                val href =
                  if (pool.defined_?)
                    // disable shortener to avoid popularity statistics
                    url.toString
                  else
                    root + "/u/" + id
                <xml:group> <a class="tiplelement" href={href} target="_blank" title={name}>{name}</a> </xml:group>
              }
            )
          ).getOrElse(Text("") )

        case x => x
      }
    }
  )

  lazy val digestedXHTML =
    (toXml \ "body").map(msgInfoTransformer).flatMap(_.child)

  private lazy val originalXml = PCDataXmlParser(text.is).openOr(<message/>)
  
  lazy val toXHTML = transformBody(digestedXHTML)
    
  lazy val toPlainTextBody = transformBody(Text(body))

  private [model] def saveTheTags() = synchronized {
    for (tag <- tagIds) {
      MessageTag.create.message(this).tag(tag).save
    }
    for (sentTo <- sentToIds)
    MessageTag.create.message(this).sentTo(sentTo).save

    for (urlId <- urlIds)
    MessageTag.create.message(this).url(urlId).save
  }

  lazy val sentToIds: List[Long] =
  (for (body <- originalXml \ "body";
        at <- body \ "at_name";
        id <- at.attribute("id")) yield id.text.toLong).toList

  lazy val urlIds: List[Long] =
  (for (body <- originalXml \ "body";
        at <- body \ "url";
        id <- at.attribute("id")) yield id.text.toLong).toList

  private var _atNameMap: Map[Long, User] = Map.empty
  private var _setNameMap = false

  private def primeNameMap(in: Map[Long, User]) = synchronized {
    _atNameMap = in
    _setNameMap = true
  }

  lazy val nameMap: Map[Long, User] = synchronized {
    if (_setNameMap) _atNameMap
    else Map(User.findAll(InRaw(User.id, sentToIds.mkString(","),
                                IHaveValidatedThisSQL("dpp", "August 23 2008"))).map(u => (u.id.is, u)) :_*)
  }

  lazy val tagIds: List[Long] =
  (for (tag <- xmlTags;
        id <- tag.attribute("id")) yield id.text.toLong).toList

  lazy val tags: List[String] =
  (for (tag <- xmlTags;
        name <- tag.attribute("name")) yield name.text).toList

  lazy val xmlTags: Seq[Node] =
  for (tags <- (originalXml \ "tags");
       tag <- tags \ "tag") yield tag

  // Define getter methods for Compass to use
  @SearchableId
  def getId:Long = id

  @SearchableProperty
  def getAuthor:Long = author.is

  // termVector=YES means that we get the word frequencies for tag clouds
  @SearchableProperty(termVector=TermVector.YES, analyzer="stemming")
  def getText:String = originalXml.text
  
  // Body without extra tags
  def getBody:String = body

  @SearchableProperty(termVector=TermVector.YES, analyzer="default")
  def getTextWords:String = originalXml.text

  @SearchableProperty(format="yyyy-MM-dd mm:ss")
  def getWhen = new java.util.Date(when.is)

  @SearchableProperty(termVector=TermVector.YES, analyzer="default")
  def getTags:String = {
    // Create a string of space-separated tags, with the spaces in each tag converted to underscores 
    val tagString: String = tags.map(x => x.split(" ").mkString("_")).mkString(" ").toLowerCase()  
    tagString
  } 

  @SearchableProperty(termVector=TermVector.YES, analyzer="pool")
  def getPool = pool.is

  /**
   * Parse and format into XML
   * Note that the text representation of the XML must be readable
   * for clients that don't support markup formatting
   * and is recommended to result in the same XML when parsed
   */                                                                                        
   
  def setTextAndTags(in: String, tags: List[Tag], metaData: Box[Node]): Box[Message] = {
    MsgParser.parseMessage(in).map{
      lst =>
      val xml = <message><body>{
            lst map {
              // get rid of extra paragraphs
              case textile: Textile => paraFixer(textile.toHtml)
            }
          }</body>
        <tags>{
            ((lst.flatMap{case Paragraph(elems, _) => {elems.flatMap { case HashTag(t) => Full(t) case _ => Empty} }  case _ => Empty})
             ::: tags).distinct.map(_.toXml)
          }</tags>{
          metaData match {
            case Full(xs) => <metadata>{xs}</metadata>
            case _ => NodeSeq.Empty
          }

        }</message>
      debug("Message.setTextAndTags(), generated XML: %s".format(xml.toString))
      this.text(xml.toString)
      this
    }
  }

  // Because Message.getClass returns a ref to Message$
  def clazz = this.getClass()

  // Get the term (i.e. word) frequencies for the word cloud from the message text
  lazy val wordFrequencies: List[(String, Int)] = {
    try {
      (for {session <- compass.map(_.openSession())} yield {
          var tx:CompassTransaction = null
          var returnValue:List[(String, Int)] = Nil

          try {
            tx = session.beginTransaction()

            val msgResource = session.getResource(clazz, id) match {
              case null =>  Message.logger.debug("Saving entity to lucene in wordFrequencies")
                session.save(this)
                session.loadResource(clazz, id)  // throws exception if not found

              case x => x
            }

            val textTermFreqs:TermFreqVector = LuceneHelper.getTermFreqVector(session, msgResource, "textWords")
            Message.logger.debug("textTermFreqs: "+textTermFreqs)

            def termsAndFreq(in: TermFreqVector) = in match {
              case null => Nil
              case tf => (tf.getTerms zip tf.getTermFrequencies).toList
            }

            returnValue = termsAndFreq(textTermFreqs)

            tx.commit();
          } catch  {
            case ce: CompassException =>
              if (tx != null) tx.rollback();
          } finally {
            session.close();
          }

          compoundStem(returnValue)
        }) openOr Nil
    } catch {
      case e => e.printStackTrace; Nil
    }

  }

  // Get the tag frequencies for the tag cloud from the message's tags
  lazy val tagFrequencies: List[(String, Int)] = {
    tags.map((_, 1)).toList
  }

}
