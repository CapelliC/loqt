/*
    pqGraphviz    : interfacing SWI-Prolog and Graphviz library

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
    Copyright (C) : 2013,2014 Carlo Capelli

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "pqGraphviz.h"
#include "lqXDot.h"
#include <QDebug>
#include <QMessageBox>

#include "SwiPrologEngine.h"

pqGraphviz::pqGraphviz()
{
}

/** must be called after a Prolog engine is available
 */
void pqGraphviz::setup() {
    lqXDot::registerMetaTypes();

    SwiPrologEngine::in_thread t;
    foreach (auto s, QString("gv_uty,termtree").split(','))
        if (!t.resource_module(s))
            QMessageBox::critical(0, "error", QString("cannot load %1").arg(s));
}

/** Include low level interface
 *  most of the following notes are drawn from documentation
 *  - [Cgraph library tutorial](http://www.graphviz.org/pdf/Agraph.pdf)
 *  - [Using Graphviz as a library](http://www.graphviz.org/doc/libguide/libguide.pdf)
 */

#undef PROLOG_MODULE
#define PROLOG_MODULE "pqGraphviz"
#include "PREDICATE.h"

#define ptrcast(Ty, Fun) inline Ty *Fun(T X) {  \
    if (Ty *p = pq_cast<Ty>(X)) return p;       \
    throw PlException("null ptr " #Ty);         \
}

ptrcast(GVC_t, gvc)
ptrcast(Agraph_t, graph)
ptrcast(Agnode_t, node)
ptrcast(Agedge_t, edge)
ptrcast(Agsym_t, sym)
ptrcast(Agrec_t, rec)

/** GVC_t *gvContext()
 *  minimal interface to Graphviz context: create context for graph construction setup and rendering
 */
PREDICATE(gvContext, 1) {
    if (GVC_t *C = gvContext())
        return PL_A1 = C;
    throw PlException("gvContext failed");
}

/** int gvFreeContext(GVC_t *gvc)
 *
 *  minimal interface to Graphviz context: call after rendering done
 *  Beware! naked pointers. Need to introduce guarded allocation.
 */
PREDICATE(gvFreeContext, 1) {
    gvFreeContext(gvc(PL_A1));
    return TRUE;
}

/** Agraph_t* G = agopen(name, type, &disc);
 *
 *  - libguide.pdf
 *
 *  The first argument is a char* giving the name of the graph; the second argument is an Agdesc t value
 *  describing the type of graph to be created. A graph can be directed or undirected. In addition, a graph can
 *  be strict, i.e., have at most one edge between any pair of nodes, or non-strict, allowing an arbitrary number
 *  of edges between two nodes.
 *
 *  If the graph is directed, the pair of nodes is ordered, so the graph can have
 *  edges from node A to node B as well as edges from B to A. These four combinations are specified by the
 *  values in Table 3. The return value is a new graph, with no nodes or edges. So, to open a graph named
 *  "network" that is directed but not strict, one would use
 *
 *  Agraph_t* G = agopen("network", Agdirected, 0);
 *
 *  The third argument is a pointer to a discipline of functions used for reading, memory, etc. If the value of 0
 *  or NULL is used, the library uses a default discipline.
 *
 *  Nodes and edges are created by the functions agnode and agedge, respectively.
 *
 *  =Graph Type         =Graph
 *  =Agundirected       =Non-strict, undirected graph
 *  =Agstrictundirected =Strict, undirected graph
 *  =Agdirected         =Non-strict, directed graph
 *  =Agstrictdirected   =Strict, directed graph
 */
PREDICATE(agopen, 4) {
    QString type_ = t2w(PL_A2); Agdesc_t type;
    if (type_ == "Agdirected") type = Agdirected; else
    if (type_ == "Agstrictdirected") type = Agstrictdirected; else
    if (type_ == "Agundirected") type = Agundirected; else
    if (type_ == "Agstrictundirected") type = Agstrictundirected; else
        throw PlException("invalid graph type");

    if (Agraph_t *G = agopen(CP(PL_A1), type, 0))
        return PL_A4 = G;

    throw PlException("agopen failed");
}

/** int agclose(Agraph_t *g);
 *
 *  Release graph memory.
 *  Beware! naked pointers. Need to introduce guarded allocation.
 */
PREDICATE(agclose, 1) {
    if (agclose(graph(PL_A1)))
        throw PlException("cannot agclose");
    return TRUE;
}

/** Agnode_t *agnode(Agraph_t *g, char *name, int cflag);
 *
 *  - libguide.pdf
 *
 *  The first argument is the graph containing the node or edge. Note that if this is a subgraph, the node or edge
 *  will also belong to all containing graphs.
 *  The second argument to agnode is the node’s name. This is a key
 *  for the node within the graph. If agnode is called twice with the same name, the second invocation will
 *  not create a new node but simply return a pointer to the previously created node with the given name. The
 *  third argument specifies whether or not a node of the given name should be created if it does not already
 *  exist.
 */
PREDICATE(agnode, 4) {
    if (Agnode_t* N = agnode(graph(PL_A1), CP(PL_A2), PL_A3))
        return PL_A4 = N;
    return false;
}

/** Agedge_t *agedge(Agraph_t*, Agnode_t*, Agnode_t*, char*, int);
 *
 *  - libguide.pdf
 *
 *  Edges are created using agedge by passing in the edge’s two nodes. If the graph is not strict, additional
 *  calls to agedge with the same arguments will create additional edges between the two nodes. The string
 *  argument allows you to supply a further name to distinguish between edges with the same head and tail.
 *  If the graph is strict, extra calls will simply return the already existing edge. For directed graphs, the first
 *  and second node arguments are taken to be the tail and head nodes, respectively. For undirected graph, they
 *  still play this role for the functions agfstout and agfstin, but when checking if an edge exists with
 *  agedge or agfindedge, the order is irrelevant. As with agnode, the final argument specifies whether
 *  or not the edge should be created if it does not yet exist
 */
PREDICATE(agedge, 6) {
    if (Agedge_t *E = agedge(graph(PL_A1), node(PL_A2), node(PL_A3), CP(PL_A4), PL_A5))
        return PL_A6 = E;
    return false;
}

/** Agsym_t *agattr(Agraph_t *g, int kind, char *name, char *value);
 *
 *  - libguide.pdf
 *
 *  Before attaching an attribute to a graph component, the code
 *  must first set up the default case. This is accomplished by a call to agattr. It takes a graph, an object
 *  type (AGRAPH, AGNODE, AGEDGE), and two strings as arguments, and return a representation of the
 *  attribute. The first string gives the name of the attribute; the second supplies the default value. The graph
 *  must be the root graph.
 */
PREDICATE(agattr, 5) {
    QString Kind = t2w(PL_A2);
    int kind;
    if (Kind == "AGRAPH") kind = AGRAPH; else
    if (Kind == "AGNODE") kind = AGNODE; else
    if (Kind == "AGEDGE") kind = AGEDGE; else
        throw PlException("invalid agattr");
    if (Agsym_t *A = agattr(graph(PL_A1), kind, CP(PL_A3), CP(PL_A4)))
        return PL_A5 = A;
    throw PlException("agattr failed");
}

/** char *agget(void *obj, char *name);
 *
 *  - libguide.pdf
 *
 *  The function agget takes a pointer to a graph component (node,
 *  edge or graph) and an attribute name, and returns the value of the attribute for the given component. Note
 *  that the function may return either NULL or a pointer to the empty string. The first value indicates that
 *  the given attribute has not been defined for any component in the graph of the given kind. Thus, if abc
 *  is a pointer to a node and agget(abc,"color") returns NULL, then no node in the root graph has a
 *  color attribute. If the function returns the empty string, this usually indicates that the attribute has been
 *  defined but the attribute value associated with the specified object is the default for the application. So, if
 *  agget(abc,"color") now returns "", the node is taken to have the default color. In practical terms,
 *  these two cases are very similar. Using our example, whether the attribute value is NULL or "", the drawing
 *  code will still need to pick a color for drawing and will probably use the default in both cases.
 */
PREDICATE(agget, 3) {
    QString value = agget(VP(PL_A1), CP(PL_A2));
    return PL_A3 = A(value);
}

static CP decoattr(CCP func, VP obj, T val) {

    if (!obj)
        throw PlException(A(QString("%1: invalid object").arg(func)));

    CP value;
    if (val.type() == PL_TERM && val.arity() == 1 && !strcmp(val.name(), "html")) {
        Agraph_t *g = agroot(obj);
        if (!g)
            throw PlException(A(QString("%1: invalid graph").arg(func)));
        return agstrdup_html(g, CP(val[1]));
        if (aghtmlstr(value))
            qDebug() << "isHtml" << value;
    }
    else
        value = CP(val);

    return value;
}

/** int agset(void *obj, char *name, char *value);
 *
 *  - libguide.pdf
 *
 *  Once the attribute has been initialized, the attribute can be set for a specific component by calling
 *  agset (void*, char*, char*)
 *  with a pointer to the component, the name of the attribute and the value to which it should be set. For
 *  example, the call
 *  agset (np, "color", "blue");
 *  sets the color of node np to "blue". The attribute value must not be NULL.
 */
PREDICATE(agset, 3) {
    VP obj = VP(PL_A1);
    CP name = CP(PL_A2);
    CP value = decoattr("agset", obj, PL_A3);
    if (agset(obj, name, value))
        throw PlException(A(QString("agset %1,%2 failed").arg(name).arg(value)));
    return TRUE;
}

/** int agsafeset(void* obj, char* name, char* value, char* def);
 *
 *  - libguide.pdf
 *
 *  For simplicity, the cgraph library provides the function
 *  agsafeset(void*, char*, char*, char*)
 *  the first three arguments being the same as those of agset. This function first checks that the named
 *  attribute has been declared for the given graph component. If it has not, it declares the attribute, using its
 *  last argument as the required default value. It then sets the attribute value for the specific component.
 */
PREDICATE(agsafeset, 4) {
    VP obj = VP(PL_A1);
    if (agsafeset(obj, CP(PL_A2), decoattr("agsafeset", obj, PL_A3), CP(PL_A4)))
        throw PlException("agsafeset failed");
    return TRUE;
}

/** Agnode_t *agfstnode(Agraph_t * g);
 *
 *  - Agraph.pdf
 *
 *  Cgraph has functions for iterating over graph objects. For example, we can scan
 *  all the edges of a graph (directed or undirected) by the following:
 *  for (n = agfstnode(g); n; n = agnxtnode(g,n)) {
 *   for (e = agfstout(g,n); e; e = agnxtout(g,e)) {
 *    // do something with e
 *   }
 *  }
 *  The functions agfstin(g,n) and afnxtin(g,e) are provided for walking in-edge lists
 */
PREDICATE(agfstnode, 2) {
    if (auto p = agfstnode(graph(PL_A1)))
        return PL_A2 = p;
    return false;
}

/** Agnode_t *agnxtnode(Agraph_t * g, Agnode_t * n);
 */
PREDICATE(agnxtnode, 3) {
    if (auto p = agnxtnode(graph(PL_A1), node(PL_A2)))
        return PL_A3 = p;
    return false;
}

/** Agedge_t *agfstout(Agraph_t * g, Agnode_t * n);
 */
PREDICATE(agfstout, 3) {
    if (auto p = agfstout(graph(PL_A1), node(PL_A2)))
        return PL_A3 = p;
    return false;
}

/** Agedge_t *agnxtout(Agraph_t * g, Agedge_t * e);
 */
PREDICATE(agnxtout, 3) {
    if (auto p = agnxtout(graph(PL_A1), edge(PL_A2)))
        return PL_A3 = p;
    return false;
}

// subgraphs

/** Agraph_t *agsubg(Agraph_t * g, char *name, int cflag);
 *
 *  - libguide.pdf
 *
 *  The first argument is the immediate parent graph; the second argument is the name of the subgraph; the final
 *  argument indicates if the subgraph should be created.
 *  Subgraphs play three roles in Graphviz. First, a subgraph can be used to represent graph structure,
 *  indicating that certain nodes and edges should be grouped together. This is the usual role for subgraphs
 *  and typically specifies semantic information about the graph components. In this generality, the drawing
 *  software makes no use of subgraphs, but maintains the structure for use elsewhere within an application.
 *  In the second role, a subgraph can provide a context for setting attributes. In Graphviz, these are often
 *  attributes used by the layout and rendering functions. For example, the application could specify that blue
 *  is the default color for nodes. Then, every node within the subgraph will have color blue. In the context of
 *  graph drawing, a more interesting example is:
 *  subgraph {
 *   rank = same; A; B; C;
 *  }
 *  This (anonymous) subgraph specifies that the nodes A, B and C should all be placed on the same rank if
 *  drawn using dot.
 *  The third role for subgraphs combines the previous two. If the name of the subgraph begins with
 *  "cluster", Graphviz identifies the subgraph as a special cluster subgraph. The drawing software will
 *  do the layout of the graph so that the nodes belonging to the cluster are drawn together, with the entire
 *  drawing of the cluster contained within a bounding rectangle.
 *
 * - Agraph.pdf
 *
 * Subgraphs are an important construct in Cgraph. They are intended for organizing
 * subsets of graph objects and can be used interchangeably with top-level graphs in
 * almost all Cgraph functions.
 * A subgraph may contain any nodes or edges of its parent. (When an edge is
 * inserted in a subgraph, its nodes are also implicitly inserted if necessary. Similarly,
 * insertion of a node or edge automatically implies insertion in all containing sub-
 * graphs up to the root.) Subgraphs of a graph form a hierarchy (a tree). Cgraph has
 * functions to create, search, and iterate over subgraphs.
 */
PREDICATE(agsubg, 4) {
    if (auto p = agsubg(graph(PL_A1), CP(PL_A2), PL_A3))
        return PL_A4 = p;
    return false;
}

/** Agraph_t *agfstsubg(Agraph_t * g);
 */
PREDICATE(agfstsubg, 2) {
    if (auto p = agfstsubg(graph(PL_A1)))
        return PL_A2 = p;
    return false;
}

/** Agraph_t *agnxtsubg(Agraph_t * subg);
 */
PREDICATE(agnxtsubg, 2) {
    if (auto p = agnxtsubg(graph(PL_A1)))
        return PL_A2 = p;
    return false;
}

/** Agraph_t *agparent(Agraph_t * g);
 *
 * - Agraph.pdf
 *
 * The function agparent returns the (sub)graph immediately containing the
 * argument subgraph. The iteration done using agfstsubg and agnxtsubg returns
 * only immediate subgraphs. To find subgraphs further down the hierarchy
 * requires a recursive search
 */
PREDICATE(agparent, 2) {
    return PL_A2 = agparent(graph(PL_A1));
}

// reusing

/** Agnode_t *agsubnode(Agraph_t * g, Agnode_t * n, int createflag);
 *
 * It is not uncommon to want to populate a subgraph with nodes and edges that
 * have already been created. This can be done using the functions agsubnode and agsubedge,
 * Agnode_t *agsubnode(Agraph_t *g, Agnode_t *n, int create);
 * Agedge_t *agsubedge(Agraph_t *g, Agedge_t *e, int create);
 * which take a subgraph, and an object from another subgraph of the same graph (or
 * possibly a top-level object) and add it to the argument subgraph if the create flag
 * is TRUE. It is also added to all enclosing subgraphs, if necessary. If the create
 * flag is FALSE, then the request is only treated as a search and returns NULL for
 * failure.
 * A subgraph can be removed by agdelsubg(g,subg) or by agclose(subg).
 */
PREDICATE(agsubnode, 4) {
    return PL_A4 = agsubnode(graph(PL_A1), node(PL_A2), PL_A3);
}

/** Agedge_t *agsubedge(Agraph_t * g, Agedge_t * e, int createflag);
 */
PREDICATE(agsubedge, 4) {
    return PL_A4 = agsubedge(graph(PL_A1), edge(PL_A2), PL_A3);
}
