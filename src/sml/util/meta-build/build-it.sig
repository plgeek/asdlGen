signature BUILD_IT =
  sig
    structure BU : BUILD_UTIL
    val rules :
      {lib:Paths.file_path list,
  includes:Paths.file_path list,
     share:Paths.file_path list,
       doc:Paths.file_path list,
 cleanable:Paths.file_path list,
       bin:Paths.file_path list} BU.M
  end
  