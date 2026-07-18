# ADR 0002: sbt 2, mdoc, and Laika integration

- Status: Accepted
- Date: 2026-07-19

## Context

DocTrellis must be a native sbt 2 plugin. It needs the consuming project's
classpath for mdoc and must render the processed Markdown without relying on an
sbt 1-only site plugin. Version and artifact compatibility were checked against
official documentation, project sources, and published artifacts on 2026-07-19.

## Decision

Use this compatibility baseline:

| Component | Version | Reason |
| --- | --- | --- |
| sbt | 2.0.3 | Current sbt 2 release and plugin target |
| JDK | 25 LTS in CI | Current LTS and compatible with sbt 2 |
| mdoc | 2.9.1 | Current release with the `sbt-mdoc_sbt2_3` plugin artifact |
| Laika | 1.3.2 | Current Scala 3 library API used by the plugin |
| Scala for VS Code modules | 3.3.8 LTS | Conservative Scala.js and WartRemover-compatible baseline |

Declare mdoc as a plugin dependency of `sbt-doctrellis` and define
`DocTrellisPlugin.requires` as `JvmPlugin && MdocPlugin`. The mdoc plugin source
shows how it adds mdoc to the consuming project and derives mdoc settings from
that project's classpath. A spike also exposed an sbt 2 integration hazard: the
plugin's dynamic `mdoc` input task delegates to `runMain`, whose client-side
execution was not reliably ordered with a following in-process Laika task on a
clean build.

Use mdoc's public `mdoc.interfaces.Mdoc` service API instead of composing that
input task. Load the implementation from an isolated loader built from the
consumer's `Compile / fullClasspath`, and pass through its compiler options,
mdoc arguments, variables, and working directory. Keep the `MdocPlugin`
requirement because it supplies the compatible implementation and user-project
settings. The scripted test verifies this path against a fresh sbt 2 build.

Implement `doctrellisBuild` as one ordered task:

1. map `mdocIn` and `mdocOut` to the DocTrellis source and intermediate
   directories;
2. evaluate Markdown through the public mdoc programmatic interface;
3. feed the intermediate directory to Laika's `Transformer` library API;
4. use GitHub-flavored Markdown and `Helium.defaults`;
5. write HTML to `target/doctrellis/site` and require `index.html`.

The side-effecting preparation, mdoc, rendering, and preview tasks explicitly
use `Def.uncached`; sbt 2 caches custom tasks by default, but these tasks manage
live files and a server rather than a serializable result.

Do not use `laika-sbt` or `sbt-typelevel-site`. They may inform user experience,
but using Laika's Scala 3 library API avoids introducing an sbt 1 plugin into an
sbt 2 build.

`doctrellisPreview` performs a fresh build and then starts a JDK HTTP server.
The first release intentionally omits file watching: a correct ordered build,
resource cleanup, and understandable bind/build errors take priority. Users
restart preview after source changes.

## Consequences

- mdoc sees the user's project dependencies and compiler settings.
- Laika is isolated behind a small library call and does not control the sbt
  build.
- The plugin requires a JVM project and explicitly opts in rather than triggering
  for every project.
- Scripted tests are the compatibility gate for the mdoc/plugin dependency.
- Live reload and incremental documentation builds remain future work.

## Sources checked

- [sbt 2.0.3 release](https://github.com/sbt/sbt/releases/tag/v2.0.3)
- [sbt plugin reference](https://www.scala-sbt.org/2.x/docs/en/reference/plugin.html)
- [mdoc installation](https://scalameta.org/mdoc/docs/installation.html)
- [mdoc 2.9.1 `MdocPlugin` source](https://github.com/scalameta/mdoc/blob/v2.9.1/mdoc-sbt/src/main/scala/mdoc/MdocPlugin.scala)
- [sbt 2 cached tasks](https://www.scala-sbt.org/2.x/docs/en/reference/cached-task.html)
- [Laika library API](https://typelevel.org/Laika/latest/02-running-laika/02-library-api.html)
- [Oracle Java support roadmap](https://www.oracle.com/java/technologies/java-se-support-roadmap.html)
