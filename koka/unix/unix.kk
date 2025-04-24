module unix

pub import basic
pub import fs
pub import pipe
pub import sync
pub import helper

pub fun unix_mini(action)
  with basicIO
  with sessionmgr(Root)
  with timeshare
  with status
  action

pub fun unix(action)
  with runState(FileSystem())
  with fileIO
  with sessionmgr(Root)
  with timeshare2
  with retro-sync
  with interruptWrite
  with status
  action()


// Session Migration
pub fun unix_sm2(action)
  with runState(FileSystem())
  with fileIO
  with sessionmgr2(Root)
  with timeshare2
  with retro-sync
  with interruptWrite
  with status
  action()

pub fun unix_sm3(action)
  with runState(FileSystem())
  with fileIO
  with sessionmgr3(Root)
  with timeshare2
  with retro-sync
  with interruptWrite
  with status
  action()

pub fun unix_sm4(action)
  with runState(FileSystem())
  with fileIO
  with sessionmgr4(Root)
  with timeshare2
  with retro-sync
  with interruptWrite
  with status
  action()

// Time Share
pub fun unix_ts3(action)
  with runState(FileSystem())
  with fileIO
  with sessionmgr2(Root)
  with timeshare3
  with retro-sync
  with interruptWrite
  with status
  action()

pub fun unix_ts4(action)
  with runState(FileSystem())
  with fileIO
  with sessionmgr2(Root)
  with timeshare3
  with retro-sync
  with interruptWrite
  with status
  action()