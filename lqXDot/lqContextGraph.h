/*
    lqXDot       : interfacing Qt and Graphviz library

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

#ifndef LQCONTEXTGRAPH
#define LQCONTEXTGRAPH

#include <functional>
#include "lqXDot.h"
#include <QObject>
#include <QMessageBox>
#include <QGraphicsItem>

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

//! get Graphviz string attributes preserving CV qualifiers
inline cstr attr_str(void* obj, cstr name) {
    return agget(obj, ccstr(name));
}

//! fetch localized string from untyped Graphviz constant
inline QString attr_qstr(void* obj, cstr name) {
    return QString::fromUtf8(agget(obj, ccstr(name)));
}


//! display a box -- TBD log to file
inline void critical(QString msg) {
    QMessageBox::critical(0, QObject::tr("Critical Error"), msg);
}

//! utility to get readable name string
inline QString gvname(void* o) {
    return QString::fromUtf8(agnameof(o));
}

//! make types cleaner - simpler
struct GV_ptr_types {

    //! context pointer
    typedef GVC_t* Cp;

    //! recursive graphs
    typedef Agraph_t* Gp;

    //! nodes collection
    typedef Agnode_t* Np;

    //! edges collection
    typedef Agedge_t* Ep;

    //! graphviz keeps strings/symbols unique (TBD exploit in scene creation)
    typedef const Agsym_t* Sp;

    //! utilities
    typedef QList<Ep> edges;
    typedef QList<Np> nodes;

    typedef std::function<void(Gp)> Gf;
    typedef std::function<void(Np)> Nf;
    typedef std::function<void(Ep)> Ef;
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

    //! check allocation for futher processing
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

    //! access in context structure
    void for_edges_out(Np n, Ef f) {
        for (Ep e = agfstout(graph, n); e; e = agnxtout(graph, e))
            f(e);
    }
    void for_nodes(Nf f) {
        for (Np n = agfstnode(graph); n; n = agnxtnode(graph, n))
            f(n);
    }
    void for_subgraphs(Gp r, Gf f) {
        for (Gp subg = agfstsubg(r); subg; subg = agnxtsubg(subg))
            f(subg);
    }

    Gp clone(Gp source);
    Np clone(Np source);
    Ep clone(Ep source);

    void depth_first(Np root, Nf nv);
    void depth_first(Np root, Nf nv, Ef ev);
    void depth_first(Gf gv, Gp g = 0);

signals:
    
public slots:

private:

    Cp context;
    Gp graph;

    Gp buffer;
    Gp buffer_();


    //! basic access to Graphviz error report system
    static QStringList errors;
    static int store_errors(char *msg);
};

#endif // LQCONTEXTGRAPH
