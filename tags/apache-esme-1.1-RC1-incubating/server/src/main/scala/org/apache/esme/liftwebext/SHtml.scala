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
        case first :: rest => first :: rest.toList.sort( (first: (String, String), second: (String, String)) => first._2.toUpperCase < second._2.toUpperCase)
        case _ => opts
      }
    else
      opts.toList.sort( (first: (String, String), second: (String, String)) => first._2.toUpperCase < second._2.toUpperCase)
}
