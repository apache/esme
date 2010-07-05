package org.apache.esme.liftwebext

import _root_.net.liftweb._
import http._
import S._
import _root_.net.liftweb.http.SHtml._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.AjaxInfo
import JE._
import JsCmds._
import _root_.scala.xml._

/**
 * Created by IntelliJ IDEA.
 * User: imtiaz
 * Date: Jun 27, 2010
 * Time: 11:40:19 AM
 * To change this template use File | Settings | File Templates.
 */

object SHtml {
  import net.liftweb.http.js.JsCmds.JsCrVar

  def ajaxSortedSelect(opts: Seq[(String, String)], excludeFirstOpt: Boolean, deflt: Box[String],
                 func: String => JsCmd, attrs: (String, String)*): Elem =
    ajaxSelect_*(getSortedOpts(opts, excludeFirstOpt), deflt, Empty, SFuncHolder(func), attrs: _*)

  def ajaxSortedSelect(opts: Seq[(String, String)], excludeFirstOpt: Boolean, deflt: Box[String],
                 jsFunc: Call, func: String => JsCmd, attrs: (String, String)*): Elem =
    ajaxSelect_*(getSortedOpts(opts, excludeFirstOpt), deflt, Full(jsFunc), SFuncHolder(func), attrs: _*)

  def ajaxUntrustedSortedSelect(opts: Seq[(String, String)], excludeFirstOpt: Boolean, deflt: Box[String],
                 func: String => JsCmd, attrs: (String, String)*): Elem =
    ajaxUntrustedSelect_*(getSortedOpts(opts, excludeFirstOpt), deflt, Empty, SFuncHolder(func), attrs: _*)

  def ajaxUntrustedSortedSelect(opts: Seq[(String, String)], excludeFirstOpt: Boolean, deflt: Box[String],
                 jsFunc: Call, func: String => JsCmd, attrs: (String, String)*): Elem =
    ajaxUntrustedSelect_*(getSortedOpts(opts, excludeFirstOpt), deflt, Full(jsFunc), SFuncHolder(func), attrs: _*)

  def ajaxUntrustedSelect(opts: Seq[(String, String)], deflt: Box[String],
                 func: String => JsCmd, attrs: (String, String)*): Elem =
    ajaxUntrustedSelect_*(opts, deflt, Empty, SFuncHolder(func), attrs: _*)

  def ajaxUntrustedSelect(opts: Seq[(String, String)], deflt: Box[String],
                 jsFunc: Call, func: String => JsCmd, attrs: (String, String)*): Elem =
    ajaxUntrustedSelect_*(opts, deflt, Full(jsFunc), SFuncHolder(func), attrs: _*)

  private def ajaxSelect_*(opts: Seq[(String, String)], deflt: Box[String],
                           jsFunc: Box[Call], func: AFuncHolder, attrs: (String, String)*): Elem = {
    val raw = (funcName: String, value: String) => JsRaw("'" + funcName + "=' + this.options[" + value + ".selectedIndex].value")
    val key = formFuncName

    val vals = opts.map(_._1)
    val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)
    fmapFunc(contextFuncBuilder(testFunc)) {
      funcName =>
              (attrs.foldLeft(<select>{opts.flatMap {case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}}</select>)(_ % _)) %
                      ("onchange" -> (jsFunc match {
                        case Full(f) => JsCrVar(key, JsRaw("this")) & deferCall(raw(funcName, key), f)
                        case _ => makeAjaxCall(raw(funcName, "this"))
                      }))
    }
  }
  private def ajaxUntrustedSelect_*(opts: Seq[(String, String)], deflt: Box[String],
                           jsFunc: Box[Call], func: AFuncHolder, attrs: (String, String)*): Elem = {
    val raw = (funcName: String, value: String) => JsRaw("'" + funcName + "=' + this.options[" + value + ".selectedIndex].value")
    val key = formFuncName

    val vals = opts.map(_._1)
    //val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)
    val testFunc = LFuncHolder(in => in match {case Nil => false case xs => func(xs)}, func.owner)
    fmapFunc(contextFuncBuilder(testFunc)) {
      import net.liftweb.http.js.JsCmds.JsCrVar
      funcName =>
              (attrs.foldLeft(<select>{opts.flatMap {case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}}</select>)(_ % _)) %
                      ("onchange" -> (jsFunc match {
                        case Full(f) => JsCrVar(key, JsRaw("this")) & deferCall(raw(funcName, key), f)
                        case _ => makeAjaxCall(raw(funcName, "this"))
                      }))
    }
  }

  private def selected(in: Boolean) = if (in) new UnprefixedAttribute("selected", "selected", Null) else Null

  private def deferCall(data: JsExp, jsFunc: Call): Call =
    Call(jsFunc.function, (jsFunc.params ++ List(AnonFunc(makeAjaxCall(data)))): _*)

  private def getSortedOpts(opts: Seq[(String, String)], excludeFirstOpt: Boolean): Seq[(String, String)] =
    if ( excludeFirstOpt )
      opts match {
        case first :: rest => first :: rest.toList.sort(_._2 < _._2)
        case _ => opts
      }
    else
      opts.toList.sort(_._2 < _._2)
}
