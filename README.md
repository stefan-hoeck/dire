# dire

A library for discrete functional reactive programming with
a focus on referential transparency.

## Building *dire*

In order to build the library, sbt version 0.13.0 or above is needed. In order
to create a single executable .jar file, use the `oneJar` command. As
multiple main classes are detected you will be asked to choose one.
The executable can then be found under

./example/target/scala-[scala-version]/dire-example[scala-version]-[version]-one-jar.jar

## Examples

The *example* subproject contains an ever growing list of simple examples and use
cases for functional reactive programming. Each of the examples extends
`scalaz.effect.SafeApp` and overrides the `runc` method of type `IO[Unit]`.
Run any of the examples from the sbt prompt like so:

```
$ sbt
> project dire-example
> run

Multiple main classes detected, select one to run:

 [1] dire.example.TimeExample
 [2] dire.example.ButtonApp
 [3] dire.example.UserBob
 [4] dire.example.ButtonGroup
 [5] dire.example.Looping
 [6] dire.example.HelloWorld
 [7] dire.example.FormattedIntegers
 [8] dire.example.TextFieldsValidated
 [9] dire.example.TextFields
 [10] dire.example.Drawing
 [11] dire.example.Animation
 [12] dire.example.URLReaderExample

Enter number:
```
Please note that some examples like HelloWorld will not run
properly from within sbt. In that case use the `oneJar` command
to create an executable as described under *Building dire*.

## Documentation

A blog about the concepts found and the progress made in *dire* can be found here:

https://efasckenoth.wordpress.com/

## Related work

The development of *dire* is strongly influenced by the following articles and projects:

*Deprecating the Observer Pattern* by I. Maier and M. Odersky:

http://infoscience.epfl.ch/record/176887/files/DeprecatingObservers2012.pdf

The *reactive* project by N. Gugenheim:

https://github.com/nafg/reactive

H. Apfelmus's blog:

http://apfelmus.nfshost.com/blog.html#functional-reactive-programming-frp

