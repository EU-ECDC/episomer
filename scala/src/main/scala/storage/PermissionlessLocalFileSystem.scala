package demy.storage

import org.apache.hadoop.fs.RawLocalFileSystem
import org.apache.hadoop.fs.Path
import org.apache.hadoop.fs.permission.FsPermission

class PermissionlessLocalFileSystem extends RawLocalFileSystem {
  override def setPermission(path: Path, permission: FsPermission): Unit = ()
}