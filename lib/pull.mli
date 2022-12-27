val duniverse :
  full:bool ->
  preserve_symlinks:bool ->
  root:Fpath.t ->
  global_state:OpamStateTypes.unlocked OpamStateTypes.global_state ->
  trim_clone:bool ->
  Duniverse.t ->
  (unit, [> Rresult.R.msg ]) result
(** [duniverse ~full ~preserve_symlinks ~root ~global_state duniverse]
    pulls duniverse repositories into the [Config.vendor_dir] of the given project [root].
    If [full] is [true] then the [vendor_dir] is entirely deleted before pulling.
    Otherwise it will traverse the subfolders corresponding to the repos in [duniverse]
    and delete those that aren't symlinks if [preserve_symlinks] is set. *)
