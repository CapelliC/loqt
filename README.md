loqt
====

Logic Objecs and Qt

+ pqSource: the main IDE component.
  Integrate the console, editing with semantic highlighting, inline help,
  in an MDI environment, with graphs for inclusions and references analysis.
  The entry point is pqSourceTest, graphs built via pqGraphviz.

+ pqConsole: reusing swipl-console in a component based environment (shared libraries)

+ lq3d: merging Qt3D and SWI-Prolog powers

+ lqXDot: just a rendering of Graphviz (via library cgraph,xdot) to Qt Graphic Scene.
  Such rendering plays a role in my next project (to be published), showcasing (SWI)Prolog direct control of Qt via reflection.

+ pqGraphviz: a component to bridge Prolog to Graphviz, built upon lqXDot

+ spqr: a basic SWI-Prolog Graphviz interface (WIP).
  Graphviz rendering by Prolog.

+ fdqueens: an example of multithreading GUI interface, with CLP(FD) processing.
  now entirely rewritten as a case study of SCXML, implemented with QStateMachine

+ lqShapes: Qt Graphics Framework available via reflection
  switch from a text based workspace to a scene graph

==========

lq3d components:

+ lq3d is a dynamically loadable component, providing Qt3D architecture ready to use in Qt GUIs
  + lq3dView holds the camera and input interfaces
  + lq3dScene holds the tree scenegraph root and the context

+ lq3d_test is a simple example
  (will) show how to merge support from pqConsole and lq3d to get a Prolog controlled 3d environment

==========

lqXDot components:

+ lqXDot is the actual Graphviz processor.
  + lqXDotScene the main component.
  + lqContextGraph shows a way to encapsulate the Graphviz pointer model in C++.

+ lqXDot_test is an utility UI,
  with basic file/mode maintenance,
  display both lqXDot/SVG rendering,
  and allow editing .dot,.gv with syntax coloring.

+ lqUty collects some basic Qt interface helpers.

==========

spqr features:

+ editing Prolog files via CodeMirror, with interface details embedded in Qt resources system (IDE handled qrc).
+ pqConsole control
+ inline plDoc help
  + The transition from QtWebKit to QWebEngine is done based on Qt versions (after Qt5.5 QWebEngine is used).

==========

lqShapes features:

+ basic shapes and properties interfaced via reflection
  + Bidirectional interaction between SWI-prolog and Qt.
  + Computing graph layouts with CLP(FD).
  + Shaping terms as nested, foldable graphs, computations as term rewrite.

+ lqShapes_test: minimal, simple applicative test interface for lqShapes
  + Single script utility.

+ lqGraphix: minimal, simple applicative interface
  + Single script utility.
  + GUI model to explore CLP applications.
