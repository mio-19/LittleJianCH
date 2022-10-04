package littlejian.examples.js

import littlejian._
import littlejian.data.*

import scala.language.implicitConversions

type JSData0[JSData] = LList[JSData] | String | Boolean
type JSData = JSData0[JSData0[Any]]
implicit def jsData0ToJSData[T](x: JSData0[T]): JSData = x.asInstanceOf[JSData]
implicit def jsDataToJSData0[T](x: JSData): JSData0[T] = x.asInstanceOf[JSData0[T]]

implicit val U$JSData: Unifier[JSData] = U$Union(U$LList(U$JSData), U$String, U$Boolean).asInstanceOf[Unifier[JSData]]