# dire

A library for discrete functional reactive programming

## Building *dire*

In order to build the library, sbt version 0.12.0 or above is needed. In order
to create a single executable .jar file, use the `one-jar` command. The executable
can then be found under

./example/target/scala-2.10/dire-example[scala-version]-[version]-one-jar.jar

## Examples

The *example* subproject contains an ever growing list of simple examples and use
cases for functional reactive programming. Each of the examples (with exception to
the `Time`-example), comes with a `run` method of type `IO[Unit]`. In order to
run an example, modify `dire.example.Main` like so:

```scala
object Main extends SafeApp {
  override def runc: IO[Unit] = ExampleName.run
}
```

where `ExampleName` is the name of the example object. Then build using command
`one-jar` and run the executable found in the location described above.

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

