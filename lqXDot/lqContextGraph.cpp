/*
    lqXDot       : interfacing Qt and Graphviz library

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016

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

#include "lqContextGraph.h"
#include "lqXDotScene.h"
#include "lqXDot_configure.h"

#include <QStack>
#include <QDebug>

/** shortcuts */
inline void OK(int rc) { Q_ASSERT(rc == 0); Q_UNUSED(rc); }

/** allocate empty
 */
lqContextGraph::lqContextGraph(QObject *parent) :
    QObject(parent),
    trace_control(0),
    context(0),
    graph(0)
{
}

/** keep pointers allocated elsewhere (Prolog, in first use case)
 */
lqContextGraph::lqContextGraph(GVC_t* context, Agraph_t *graph, QObject *parent) :
    QObject(parent),
    trace_control(0),
    context(context),
    graph(graph)
{
}

/** release resources
 */
lqContextGraph::~lqContextGraph()
{
    clear();
}

/** hopefully remove cleanup all memory used
 */
void lqContextGraph::clear() {
    if (context) {
        if (graph) {
            gvFreeLayout(context, graph);
            agclose(graph);
            graph = 0;
        }

        // no layout performed on buffers
        foreach(auto p, buffers)
            agclose(p.spare_graph);
        buffers.clear();

        gvFreeContext(context);
        context = 0;
    }
}

/** keep a static list to collect errors from global handler
 */
QStringList lqContextGraph::errors;
const int max_errors = 10;

int lqContextGraph::store_errors(char *msg) {
    if (errors.count() < max_errors)
        errors.append(QString::fromUtf8(msg));
    return 0;
}

/** perform task
 *  display a box with errors (upto max_errors), if any
 *  return true if no error happened
 */
bool lqContextGraph::run_with_error_report(std::function<QString()> worker) {

    QString err;
    try {
        errors.clear();
        agusererrf savef = agseterrf(store_errors);
        err = worker();
        agseterrf(savef);
    }
    catch(...) {
        err = tr("Exception!");
    }
    if (!err.isEmpty() || !errors.isEmpty()) {
        critical(err + "\n" + errors.join("\n"));
        return false;
    }
    return true;
}

/** ensure context available for subsequent operations
 */
bool lqContextGraph::in_context() {

    if (oktrace(tf_in_context))
        qDebug() << "in_context" << CVP(this);

    if (!context && (context = gvContext()) == 0) {
        critical(tr("gvContext() failed"));
        return false;
    }

    if (oktrace(tf_version))
        qDebug() << "gvcVersion" << gvcVersion(context);

    return true;
}

/** perform required layout with basic error handling
 */
bool lqContextGraph::layout(QString algo) {
    if (!in_context())
        return false;

    // workaround multiple boxes on root, suggested by Erg
    agset(graph, ccstr("_draw_"), ccstr(""));

    if (gvLayout(context, graph, algo.toUtf8().data()) == 0) {
        last_layout = algo;
        return true;
    }
    critical(tr("gvLayout(%1) failed").arg(algo));
    return false;
}

/** perform rendering with basic error handling
 */
bool lqContextGraph::render(QString algo) {
    if (!in_context())
        return false;
    if (gvRender(context, graph, algo.toUtf8().data(), 0) == 0) {
        last_render = algo;
        return true;
    }
    critical(tr("gvRender(%1) failed on %2").arg(algo, last_layout));
    return false;
}

/** repeat layout and render operations with basic error handling
 */
bool lqContextGraph::repeatOperations() {
    if (freeLayout()) {
        if (layout(last_layout))
            if (render(last_render))
                return true;
    }
    critical(tr("repeatOperations failed on %1,%2").arg(last_layout, last_render));
    return false;
}

/** release layout memory with basic error handling
 */
bool lqContextGraph::freeLayout() {
    if (gvFreeLayout(context, graph)) {
        critical(tr("gvFreeLayout failed"));
        return false;
    }
    return true;
}

/** read from file
 */
bool lqContextGraph::parse(FILE *fp) {
    return (graph = agread(fp, 0)) ? true : false;
}

/** read from string
 */
bool lqContextGraph::parse(QString script) {
    clear();
    return in_context() && (graph = agmemread(script.toUtf8())) ? true : false;
}

/** allocate a spare graph, to store elements
 *  on structure change (folding/unfolding)
 */
lqContextGraph::buffer* lqContextGraph::buff(Np n, bool decl_attrs) {
    buffer *buf = 0;

    QString N = gvname(n);
    t_buffers::iterator p = buffers.find(N);
    if (p != buffers.end())
        buf = &p.value();
    else {
        bool strict = agisstrict(graph);
        Agdesc_t desc = agisdirected(graph) ? (strict ? Agstrictdirected : Agdirected) : (strict ? Agstrictundirected : Agundirected);
        buffer b;
        b.spare_graph = agopen(qcstr(QString("#buffer#%1#%2").arg(buffers.count()).arg(N)), desc, 0);
        buf = &buffers.insert(N, b).value();
    }

    if (decl_attrs) {
        declattrs(graph, buf->spare_graph, AGRAPH);
        declattrs(graph, buf->spare_graph, AGNODE);
        declattrs(graph, buf->spare_graph, AGEDGE);
    }

    return buf;
}

void lqContextGraph::declattrs(Gp src, Gp dst, int kind) {
    for (Sp sym = 0; (sym = agnxtattr(src, kind, sym)); )
        agattr(dst, kind, sym->name, sym->defval);
}

/** a simple depth first visit starting on <root>
 */
void lqContextGraph::depth_first(Np root, Nf nv, Gp g) {
    depth_first(root, nv, [](Ep){}, g);
}

/** a simple depth first visit starting on <root>
 */
void lqContextGraph::depth_first(Np root, Nf nv, Ef ev, Gp g) {
    QStack<Np> s; s.push(root);
    nodes visited;
    while (!s.isEmpty()) {
        Np n = s.pop();
        if (!visited.contains(n)) {
            visited << n;
            nv(n);
            for_edges_out(n, [&](Ep e){ ev(e); s.push(e->node); }, g);
        }
    }
}

/** a simple depth first visit
 */
void lqContextGraph::depth_first(Gf gv, Gp root) {
    QStack<Gp> s; s.push(root ? root : graph);
    while (!s.isEmpty()) {
        Gp n = s.pop();
        gv(n);
        for_subgraphs([&](Gp g){ s.push(g); }, n);
    }
}

/** this test must be performed only on 'user available' nodes
 */
bool lqContextGraph::is_folded(Np n) const {
    Q_ASSERT(agraphof(n) == graph);
    return buffers.contains(gvname(n));
}

/** make structural changes required to fold node <n>
 *  all nodes reachable from n are merged to n (removed and buffered in context),
 *  and edges are routed to arguably preserve the structure
 */
GV_ptr_types::Gp lqContextGraph::fold(Np n) {
    Q_ASSERT(!is_folded(n));

    QSet<Ep> edel;
    QSet<Np> ndel;

    buffer* B = buff(n, true);

    Nf N = [&](Np v) {
        if (Gp s = find_inner_subgraph(v)) {
            Gp S = agsubg(B->spare_graph, agnameof(s), 1);
            copy(v, S);
        }
        else
            copy(v, B->spare_graph);
        if (v != n)
            ndel << v;
    };
    Ef E = [&](Ep e) {
        copy(e, B->spare_graph);
        edel << e;
    };
    depth_first(n, N, E);

    foreach(auto x, edel)
        OK(agdeledge(graph, x));
    foreach(auto x, ndel) {
        for_edges_in(x, [&](Ep e) {
            QString tn = gvname(agtail(e));
            if (!agnode(B->spare_graph, qcstr(tn), 0)) {
                QString cn = gvname(e);
                QString hn = gvname(aghead(e));
                Ep E = agedge(graph, agtail(e), n, 0, 1);
                agsafeset(E, ccstr("style"), ccstr("dotted"), ccstr(""));
                B->fake_edges << fake_edge {tn, hn, cn};
            }
        });
        OK(agdelnode(graph, x));
    }
    return B->spare_graph;
}

/** make structural changes required to unfold node <n>
 */
void lqContextGraph::unfold(Np n) {
    Q_ASSERT(is_folded(n));

    buffer* B = buff(n, true);

    Np t = agnode(B->spare_graph, agnameof(n), 0);
    OK(agcopyattr(t, n));

    Nf N = [&](Np v) {
        if (Gp s = find_inner_subgraph(v, B->spare_graph)) {
            Gp S = agsubg(graph, agnameof(s), 1);
            copy(v, S);
        }
        else
            copy(v, graph);
    };
    Ef E = [&](Ep e) {
        copy(e, graph);
    };
    depth_first(t, N, E, B->spare_graph);

    foreach(auto i, B->fake_edges) {
        Np H = agnode(graph, qcstr(i.n_head), 0);
        Np T = agnode(graph, qcstr(i.n_tail), 0);
        Ep E = agedge(graph, T, n, 0, 0);
        OK(agdeledge(graph, E));
        agedge(graph, T, H, qcstr(i.n_save), 1);
    }

    OK(agclose(B->spare_graph));
    buffers.remove(gvname(n));
}

/** make a copy of node <n> with attributes
 *  switch to alternative buffer
 */
GV_ptr_types::Np lqContextGraph::copy(Np n, Gp g) {
    Q_ASSERT(g);
    Q_ASSERT(agraphof(n) != g);
    Np t = agnode(g, agnameof(n), 1);
    OK(agcopyattr(n, t));
    return t;
}

GV_ptr_types::Ep lqContextGraph::copy(Ep e, Gp g, bool nodes) {
    Q_ASSERT(g);
    Q_ASSERT(agraphof(e) != g);
    Q_ASSERT(nodes);
    Q_UNUSED(nodes);
    Ep t = agedge(g, copy(agtail(e), g), copy(aghead(e), g), agnameof(e), 1);
    OK(agcopyattr(e, t));
    return t;
}

void lqContextGraph::dump(QString m) {
    qDebug() << m;
    depth_first([&](Gp t) { qDebug() << "graph" << gvname(t) /* << CVP(find_graph(t)) */; });
    qDebug() << "nodes";
    for_nodes([&](Np n) {
        qDebug() << "node" << gvname(n) /*<< CVP(find_node(n))*/;
        for_edges_out(n, [&](Ep e) {
            qDebug() << "edge" << gvname(e) /*<< CVP(find_edge(e))*/ << "to" << gvname(e->node) /* << CVP(find_node(e->node)) */;
        });
    });
}

/** I have not yet found a better way,
 *  so make a slow linear search by now
 */
GV_ptr_types::Gp lqContextGraph::find_inner_subgraph(Np s, Gp g) {
    if (!g) g = graph;
    Gp found = 0;
l:  for (Gp subg = agfstsubg(g); subg; subg = agnxtsubg(subg))
        for (Np n = agfstnode(subg); n; n = agnxtnode(subg, n))
            if (n == s) {
                g = found = subg;
                goto l;
            }
    return found;
}

/** attempt to remove all attrs created by XDOT rendering
 */
void lqContextGraph::clearXDotAttrs() {
    typedef lqXDotScene S;
    for_nodes([this](Np n) {
        S::clear_XDotAttrs(n, S::x_attrs_node);
        for_edges_out(n, [](Ep e) {
            S::clear_XDotAttrs(e, S::x_attrs_edge);
        });
    });
}

/** display a messagebox, if not disabled
 */
void GV_ptr_types::critical(QString msg) {
    qDebug() << msg;
    using namespace configure_behaviour;
    if (!option_is_on(no_box_on_render_errors))
        QMessageBox::critical(0, QObject::tr("Critical Error"), msg);
}
