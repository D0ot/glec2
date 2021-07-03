package glec2

case class CoreParams(xlen : Int, rv64: Boolean){
  val name: String = "GLEC2"
  // currently it is just used as memory
  val l1cacheSize = 8192;
  val d1cacheSize = 8192;
  
  val pcWidth = 32;

  val pcInitVal = 0;

  val bypass = true
}
