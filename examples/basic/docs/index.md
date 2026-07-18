# DocTrellis Basic Example

This page is processed by mdoc before Laika renders it with the Helium theme.

```scala mdoc
val tools = List("mdoc", "Laika", "Helium")
tools.map(_.toUpperCase).mkString(" + ")
```

The generated result above proves that the Markdown code was compiled and
evaluated rather than copied directly into the HTML site.
