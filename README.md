# Redbook exercises and listings

## Usage with Emacs

Create a .ensime file at the root of your Scala project using sbt ensimeConfig
and sbt ensimeConfigProject at the shell.

Run M-x ensime within Emacs to start an ENSIME session.

Each Scala project uses a dedicated ENSIME session, so you only need to run M-x
ensime once per project. Any Scala files you create or visit within the project
will automatically use ENSIME for the remainder of your editing session.
