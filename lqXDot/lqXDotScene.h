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

#ifndef LQXDOTSCENE_H
#define LQXDOTSCENE_H

#include <QGraphicsScene>
#include <QGraphicsItem>
#include <QStateMachine>
#include <QPointer>

#include "lqContextGraph.h"
#include "lqAobj.h"

#include <graphviz/xdot.h>

/** translate a Graphviz graph to a QGraphicScene display graph
  * each graph object get its concrete representation as QGraphicsItemGroup
  */
class LQXDOTSHARED_EXPORT lqXDotScene : public QGraphicsScene, public GV_ptr_types
{
    Q_OBJECT

    Q_PROPERTY(bool truecolor READ truecolor WRITE setTruecolor)
    Q_PROPERTY(QString imagepath READ imagepath WRITE setImagepath)

public:

    lqXDotScene(lqContextGraph *cg);

    bool truecolor() const { return truecolor_; }
    void setTruecolor(bool value) { truecolor_ = value; }

    QString imagepath() const { return imagepath_; }
    void setImagepath(QString value) { imagepath_ = value; }

    static QColor parse_color(QString color, bool truecolor);

    // mapping visual objects
    enum { agptr };
    static Np it_node(QGraphicsItem* i);
    static Ep it_edge(QGraphicsItem* i);
    static Gp it_graph(QGraphicsItem* i);

    lqNode *find_node(Np obj) const;
    QGraphicsItemGroup *find_edge(Ep obj) const;
    QGraphicsItemGroup *find_graph(Gp obj) const;

    typedef QList<QGraphicsItem*> l_items;

    //! change the content to get node folded
    bool fold(lqNode* node);

    //! dump to debugger output
    void dump(QString m) const;


    QRectF graph_bb(Gp graph);

signals:

    void setup_completed();

public slots:

private:

    //! context/graph for root object
    QPointer<lqContextGraph> cg;

    // when enabled, accept colors with alpha component
    bool truecolor_;

    // required when displaying images in rendered graph
    QString imagepath_;

    // constructing visual objects
    QGraphicsItem* add_node(Np n);
    QGraphicsItem* add_edge(Ep e);
    void subgraphs(Gp graph, qreal off_z);

    enum x_attrs {
        _draw_ = 1 << 0,
        _ldraw_ = 1 << 1,
        _hdraw_ = 1 << 2,
        _tdraw_ = 1 << 3,
        _hldraw_ = 1 << 4,
        _htdraw_ = 1 << 5,

        x_attrs_node = _draw_|_ldraw_,
        x_attrs_edge = x_attrs_node|_hdraw_|_tdraw_|_hldraw_|_htdraw_,
        x_attrs_graph = _draw_|_ldraw_
    };
    l_items build_graphic(void *obj, int ops);

    l_items build_graphic(Np obj) { return build_graphic(obj, x_attrs_node); }
    l_items build_graphic(Ep obj) { return build_graphic(obj, x_attrs_edge); }
    l_items build_graphic(Gp obj) { return build_graphic(obj, x_attrs_graph); }

    QRectF bb_rect(void *obj, int ops);
    QRectF bb_rect(Agnode_t *node) { return bb_rect(node, x_attrs_node); }

    typedef QVector<QPointF> t_poly;
    t_poly poly_spec(const xdot_polyline& l) const {
        t_poly pts;
        for (int c = 0; c < l.cnt; ++c)
            pts.append(QPointF(l.pts[c].x, cy(l.pts[c].y)));
        return pts;
    }

    //! parse font spec specifier
    static QString font_spec(cstr fontname) {
        QString family(fontname);
        int sep = family.indexOf('-');
        if (sep > 0)
            family = QString("%1 [%2]").arg(family.left(sep), family.mid(sep + 1));
        return family;
    }

    QRectF rect_spec(const xdot_rect& r) const {
        return QRectF(QPointF(r.x - r.w, cy(r.y + r.h)), QSize(r.w * 2, r.h * 2));
    }

    static void perform_attrs(void* obj, int attrs, std::function<void(const xdot_op& op)> worker);

    // map graphviz coordinates to scene
    QRectF bbscene;
    qreal cy(qreal y) const { return bbscene.height() - y; }

public slots:
    void msg(QString);
};

inline Agnode_t *lqXDotScene::it_node(QGraphicsItem* i) {
    lqNode* n = ancestor<lqNode>(i);
    return n ? n->data(agptr).value<Agnode_t*>() : 0;
}
inline Agedge_t *lqXDotScene::it_edge(QGraphicsItem* i) {
    QGraphicsItemGroup *g = ancestor<QGraphicsItemGroup>(i);
    return g ? g->data(agptr).value<Agedge_t*>() : 0;
}
inline Agraph_t *lqXDotScene::it_graph(QGraphicsItem* i) {
    QGraphicsItemGroup *g = ancestor<QGraphicsItemGroup>(i);
    return g ? g->data(agptr).value<Agraph_t*>() : 0;
}

#endif // LQXDOTSCENE_H
