loqt: Logic Objecs and Qt
==========

+ build SWI-Prolog from source (see http://www.swi-prolog.org/build/Debian.html)
+ install Graphviz libraries *graphviz-dev*

==========

+ pqSource: the main IDE component.
  Integrate the console, editing with semantic highlighting, inline help,
  in an MDI environment, with graphs for inclusions and references analysis.
  The entry point is pqSourceTest, graphs built via pqGraphviz.

+ pqConsole: reusing swipl-console in a component based environment (shared libraries)

+ lqXDot: rendering of Graphviz (via library cgraph,xdot) to Qt Graphic Scene.

+ pqGraphviz: a component to bridge Prolog to Graphviz, built upon lqXDot

+ lqXDot is the actual Graphviz processor.
  + lqXDotScene the main component.
  + lqContextGraph shows a way to encapsulate the Graphviz pointer model in C++.

+ lqXDot_test is an utility UI,
  with basic file/mode maintenance,
  display both lqXDot/SVG rendering,
  and allow editing .dot,.gv with syntax coloring.

+ lqUty collects some basic Qt interface helpers.
