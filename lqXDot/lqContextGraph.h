/*
    lqXDot       : interfacing Qt and Graphviz library

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016,2017,2018

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

#ifndef LQCONTEXTGRAPH_H
#define LQCONTEXTGRAPH_H

#include "lqXDot.h"
#include <QObject>
#include <QMessageBox>
#include <QGraphicsItem>
#include <QHash>
#include <functional>

//! utility to get an applicative meaningful pointer from nested graphic
template<class T> inline T* ancestor(QGraphicsItem* i) {
    for ( ; i; i = i->parentItem())
        if (T *a = qgraphicsitem_cast<T*>(i))
            return a;
    return 0;
}

//! introduce basic CV preserving interface to Graphviz
typedef const char* cstr;

//! since graphviz doesn't qualify const char* pointer, here is an utility to keep code cleaner
inline char* ccstr(cstr s) {
    return const_cast<char*>(s);
}

//! ditto, about QString
inline char* qcstr(QString s) {
    return s.toUtf8().data();
}

//! get Graphviz string attributes preserving CV qualifiers
inline cstr attr_str(void* obj, cstr name) {
    return agget(obj, ccstr(name));
}

//! get Graphviz bool attribute preserving CV qualifiers
inline bool attr_bool(void *obj, cstr name) {
    QString a = attr_str(obj, name);
    return a == "yes" || a == "true";
}

//! fetch localized string from untyped Graphviz constant
inline QString attr_qstr(void* obj, cstr name) {
    return QString::fromUtf8(agget(obj, ccstr(name)));
}

//! utility to get readable name string
inline QString gvname(void* o) {
    return QString::fromUtf8(agnameof(o));
}

//! make types cleaner - simpler
struct LQXDOTSHARED_EXPORT GV_ptr_types {

    //! context pointer
    typedef GVC_t* Cp;

    //! recursive graphs
    typedef Agraph_t* Gp;

    //! nodes collection
    typedef Agnode_t* Np;

    //! edges collection
    typedef Agedge_t* Ep;

    //! graphviz keeps strings/symbols unique (TBD exploit in scene creation)
    typedef Agsym_t* Sp;

    //! utilities
    typedef QList<Ep> edges;
    typedef QList<Np> nodes;

    //! functors needed to access graph structure
    typedef std::function<void(Gp)> Gf;
    typedef std::function<void(Np)> Nf;
    typedef std::function<void(Ep)> Ef;

    //! utility to make a pointer
    typedef const void* CVP;

    //! enable selecting debug output
    enum trace_flags {
        tf_in_context,
        tf_version,
        tf_create,
        tf_delete,
        tf_add_graph,
        tf_add_node,
        tf_add_edge,
        tf_layout,
        tf_render,
        tf_parseXDot,
        tf_perform_attrs,
        tf_perform_attrs_xdot_op,
        tf_show_node_type,
        tf_redo_objects
    };

    //! display a box -- TBD log to file
    static void critical(QString msg);
};

/** a container for paired Graphviz pointers
 */
class LQXDOTSHARED_EXPORT lqContextGraph : public QObject, public GV_ptr_types
{
    Q_OBJECT

public:

    explicit lqContextGraph(QObject *parent = 0);
    lqContextGraph(Cp context, Gp graph, QObject *parent = 0);
    ~lqContextGraph();

public:

    operator Cp () const { return context; }
    operator Gp () const { return graph; }

    QString last_layout;
    QString last_render;

    //! release Graphviz allocated resources
    void clear();

    //! report error handling: GV error callback *must* be a pointer to function
    static bool run_with_error_report(std::function<QString()> worker);

    //! check allocation for further processing
    bool in_context();

    //! perform gvLayout(algo)
    bool layout(QString algo);

    //! perform gvRender(algo)
    bool render(QString algo);

    //! repeat previous operation on eventually updated graph
    bool repeatOperations();

    //! perform gvFreeLayout
    bool freeLayout();

    //! serial input - from FILE
    bool parse(FILE *f);

    //! serial input - from string
    bool parse(QString f);

    //! iterate functor <f> on each edge exiting <n> (access in context structure)
    void for_edges_out(Np n, Ef f, Gp g = 0) {
        if (g == 0) g = graph;
        for (Ep e = agfstout(g, n); e; e = agnxtout(g, e))
            f(e);
    }

    //! iterate functor <f> on each edge entering <n> (access in context structure)
    void for_edges_in(Np n, Ef f, Gp g = 0) {
        if (g == 0) g = graph;
        for (Ep e = agfstin(g, n); e; e = agnxtin(g, e))
            f(e);
    }

    //! iterate functor <f> on each node of graph <g> (access in context structure)
    void for_nodes(Nf f, Gp g = 0) {
        if (g == 0) g = graph;
        for (Np n = agfstnode(g); n; n = agnxtnode(g, n))
            f(n);
    }

    //! iterate functor <f> on subgraphs of <r>
    void for_subgraphs(Gf f, Gp r = 0) {
        if (r == 0) r = graph;
        for (Gp subg = agfstsubg(r); subg; subg = agnxtsubg(subg))
            f(subg);
    }

    //!  visiting in context structure
    void depth_first(Np root, Nf nv, Gp g = 0);
    void depth_first(Np root, Nf nv, Ef ev, Gp g = 0);
    void depth_first(Gf gv, Gp g = 0);

    //! structure manipulation - status check of node <n>
    bool is_folded(Np n) const;

    //! structure manipulation - fold node <n>
    Gp fold(Np n);

    //! structure manipulation - unfold node <n>
    void unfold(Np n);

    //! debugging utility, dump graph structure to trace
    void dump(QString m);

    //! make a slow linear search by now
    Gp find_inner_subgraph(Np n, Gp g = 0);

    //! attempt to remove all attrs created by XDOT rendering
    void clearXDotAttrs();

    //! enable selective tracing from scene setup
    int trace_control;
    bool oktrace(trace_flags t) { return (trace_control & (1 << t)) == (1 << t); }

signals:

public slots:

private:

    Cp context;
    Gp graph;
    struct fake_edge { QString n_tail, n_head, n_save; };

    struct buffer {
        //! store copies of deleted nodes
        Gp spare_graph;
        //! remember fake new edges
        QList<fake_edge> fake_edges;
    };

    typedef QHash<QString, buffer> t_buffers;

    t_buffers buffers;

    buffer* buff(Np n, bool decl_attrs = false);
    Np copy(Np n, Gp g = 0);
    Ep copy(Ep e, Gp g = 0, bool nodes = true);

    //! basic access to Graphviz error report system
    static QStringList errors;
    static int store_errors(char *msg);

    static void declattrs(Gp src, Gp dst, int kind);
};

#endif // LQCONTEXTGRAPH_H
