namespace Sys
  constant copyright = LF sys_copyright ()
  constant version = LF sys_version ()
  constant build = LF sys_build ()
  constant buildDate = LF sys_build_date ()
  constant buildTime = LF sys_build_time ()

  /* TODO: change this when properties are available for namespaces. */
  function path () = LF sys_path ()
  /*
  property path
    get = LF sys_path ()
  end
  */

end
