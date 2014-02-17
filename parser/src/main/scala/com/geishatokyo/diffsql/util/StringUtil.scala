package com.geishatokyo.diffsql.util

/**
 * Created by takeshita on 14/02/17.
 */
object StringUtil {

  def escape(sql : String) = {
    sql.replace("'","\\'")
  }

}
