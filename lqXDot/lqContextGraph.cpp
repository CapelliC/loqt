/*
    pqGraphviz   : interfacing SWI-Prolog and Graphviz library

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013, Carlo Capelli

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
#include <QStack>

lqContextGraph::lqContextGraph(QObject *parent) :
    QObject(parent), context(0), graph(0), buffer(0)
{
}
lqContextGraph::lqContextGraph(GVC_t* context, Agraph_t *graph, QObject *parent) :
    QObject(parent), context(context), graph(graph), buffer(0)
{
}

lqContextGraph::~lqContextGraph()
{
    clear();
}

/** hopefully remove cleanup all memory used
 */
void lqContextGraph::clear()
{
    if (context) {
        if (graph) {
            gvFreeLayout(context, graph);
            agclose(graph);
            graph = 0;
        }
        if (buffer) {
            agclose(buffer);
            buffer = 0;
        }
        gvFreeContext(context);
        context = 0;
    }
}

QStringList lqContextGraph::errors;

int lqContextGraph::store_errors(char *msg) {
    if (errors.count() < 10)
        errors.append(QString::fromUtf8(msg));
    return 0;
}

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

bool lqContextGraph::in_context()
{
    if (!context && (context = gvContext()) == 0) {
        critical(tr("gvContext() failed"));
        return false;
    }
    return true;
}

bool lqContextGraph::layout(QString algo)
{
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

bool lqContextGraph::render(QString algo)
{
    if (!in_context())
        return false;
    if (gvRender(context, graph, algo.toUtf8().data(), 0) == 0) {
        last_render = algo;
        return true;
    }
    critical(tr("gvRender(%1) failed on %2").arg(algo, last_layout));
    return false;
}

bool lqContextGraph::repeatOperations()
{
    if (freeLayout()) {
        if (layout(last_layout))
            if (render(last_render))
                return true;
    }
    critical(tr("repeatOperations failed on %1,%2").arg(last_layout, last_render));
    return false;
}

bool lqContextGraph::freeLayout()
{
    if (gvFreeLayout(context, graph)) {
        critical(tr("gvFreeLayout failed"));
        return false;
    }
    return true;
}

bool lqContextGraph::parse(FILE *fp) {
    return (graph = agread(fp, 0)) ? true : false;
}
bool lqContextGraph::parse(QString script) {
    return (graph = agmemread(script.toUtf8())) ? true : false;
}

typedef lqContextGraph cg;

cg::Gp cg::clone(Gp source) {
    return source;
}
cg::Np cg::clone(Np source) {
    Np target = agnode(buffer_(), agnameof(source), 1);
    Q_ASSERT(target);

    int c = agcopyattr(source, target);
    Q_ASSERT(c == 0);
    return target;
}
cg::Ep cg::clone(Ep source) { return source; }

cg::Gp cg::buffer_() {
    if (!buffer) {
        bool strict = agisstrict(graph);
        Agdesc_t desc = agisdirected(graph) ? (strict ? Agstrictdirected : Agdirected) : (strict ? Agstrictundirected : Agundirected);
        buffer = agopen(ccstr("#buffer#"), desc, 0);

        for (Agsym_t* sym = 0; (sym = agnxtattr(graph, AGNODE, sym)); )
            agattr(buffer, AGNODE, sym->name, sym->defval);
    }
    Q_ASSERT(buffer);
    return buffer;
}

void cg::depth_first(Np root, Nf nv) {
    depth_first(root, nv, [](Ep){});
}
void cg::depth_first(Np root, Nf nv, Ef ev) {
    QStack<Np> s; s.push(root);
    nodes visited;
    while (!s.isEmpty()) {
        Np n = s.pop();
        if (!visited.contains(n)) {
            visited << n;
            nv(n);
            for_edges_out(n, [&](Ep e){ ev(e); s.push(e->node); });
        }
    }
}

void cg::depth_first(Gf gv, Gp root) {
    QStack<Gp> s; s.push(root ? root : graph);
    while (!s.isEmpty()) {
        Gp n = s.pop();
        gv(n);
        for_subgraphs(n, [&](Gp g){ s.push(g); });
    }
}

/*
QStack<Gp> s;
s.push(*cg);
while (!s.isEmpty()) {
    Gp t = s.pop();
    qDebug() << "graph" << gvname(t) << CVP(find_graph(t));
    cg->for_subgraphs(t, [&](Gp subg) { s.push(subg); });
}
*/
