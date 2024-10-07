open Prims
let (autoload_plugin : Prims.string -> Prims.bool) =
  fun ext ->
    (let uu___1 = FStar_Compiler_Debug.any () in
     if uu___1
     then
       FStar_Compiler_Util.print1
         "Trying to find a plugin for extension %s\n" ext
     else ());
    (let uu___1 = FStar_Options.find_file (Prims.strcat ext ".cmxs") in
     match uu___1 with
     | FStar_Pervasives_Native.Some fn ->
         ((let uu___3 = FStar_Compiler_Debug.any () in
           if uu___3
           then FStar_Compiler_Util.print1 "Autoloading plugin %s ...\n" fn
           else ());
          FStar_Compiler_Plugins_Base.load_tactics [fn];
          true)
     | FStar_Pervasives_Native.None -> false)