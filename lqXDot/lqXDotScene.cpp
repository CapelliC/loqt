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

#include "lqXDotScene.h"
#include "lqLogger.h"
#include "lqAniMachine.h"
#include "lqXDotView.h"
#include "make_nop.h"

#include "lqXDot_configure.h"
int lqXDotScene::configure_behaviour;

#include <QTime>
#include <QDebug>
#include <QCheckBox>
#include <QFinalState>
#include <QFontMetrics>
#include <QSignalMapper>
#include <QStateMachine>
#include <QPropertyAnimation>
#include <QAbstractTransition>
#include <QGraphicsProxyWidget>
#include <QParallelAnimationGroup>
#include <QGraphicsSceneMouseEvent>

/** need some plane height offset because nodes and edges overlap
 *  place nodes on top
 */
const int Z_NODE = 2;
const int Z_EDGE = 1;
const int Z_FOLD = Z_NODE + 1;

inline qreal dz(qreal &v) { v += .0001; return v; }

lqXDotScene::lqXDotScene(lqContextGraph *cg) : cg(cg),
    truecolor_()
{
}

/** factory interface
 */
lqNode* lqXDotScene::build_node(Np n, l_items items) {
    Q_UNUSED(n)
    return new lqNode(this, items);
}
lqEdge* lqXDotScene::build_edge(Ep e, l_items items) {
    Q_UNUSED(e)
    return new lqEdge(this, items);
    //return createItemGroup(items);
}
lqGraph* lqXDotScene::build_subgraph(Gp g, l_items items) {
    Q_UNUSED(g);
    return new lqGraph(this, items);
}

void lqXDotScene::build()
{
    truecolor_ = attr_bool(Gp(*cg), "truecolor");
    imagepath_ = attr_str(Gp(*cg), "imagepath");

    subgraphs(Gp(*cg), 1);

    qreal z_node = Z_NODE, z_edge = Z_EDGE;

    cg->for_nodes([&](Np n) {
        if (auto N = add_node(n)) {
            N->setZValue(dz(z_node));
            cg->for_edges_out(n, [&](Ep e) {
                if (auto E = add_edge(e)) E->setZValue(dz(z_edge));
            });
        }
    });

    emit setup_completed();
}

template<class T> inline T* _find(std::function<bool(T*)> f, lqXDotScene::l_items items)
{
    foreach(QGraphicsItem *i, items)
        if (T* I = qgraphicsitem_cast<T*>(i))
            if (f(I))
                return I;
    return 0;
}

lqNode *lqXDotScene::find_node(Agnode_t *obj) const
{
    return _find<lqNode>([this, obj](lqNode *i) {
        return it_node(i) == obj;
    }, items());
}
/*
QGraphicsItemGroup *lqXDotScene::find_edge(Agedge_t *obj) const
{
    return _find<QGraphicsItemGroup>([this, obj](QGraphicsItem *i) {
        return it_edge(i) == obj;
    }, items());
}
QGraphicsItemGroup *lqXDotScene::find_graph(Agraph_t *obj) const
{
    return _find<QGraphicsItemGroup>([this, obj](QGraphicsItem *i) {
        return it_graph(i) == obj;
    }, items());
}
*/

/** translate a color specification string to Qt class QColor
 *  this decode function doesn't depend on XDOT
 */
QColor lqXDotScene::parse_color(QString color, bool truecolor)
{
    QColor r;
    if (QColor::isValidColor(color))
        r = QColor(color);
    else if (int n = color.count()) // not empty
        if (n == 9 && truecolor && color[0] == '#') {
            static uchar charv[128];
            if (!charv['A']) {
                for (int c = '0'; c <= '9'; ++c)
                    charv[c] = c - '0';
                for (int c = 'a'; c <= 'f'; ++c)
                    charv[c] = 10 + c - 'a';
                for (int c = 'A'; c <= 'F'; ++c)
                    charv[c] = 10 + c - 'A';
            }
            auto chex = [&](int p) {
                return charv[uchar(color[p].toLatin1())] * 16 + charv[uchar(color[p+1].toLatin1())];
            };
            int R = chex(1),
                G = chex(3),
                B = chex(5),
                A = chex(7);
            r.setRgb(R, G, B, A);
        }
    return r;
}

/** translate <n> XDOT attributes to graphics, add some default behaviour
 */
QGraphicsItem* lqXDotScene::add_node(Np n)
{
    QString N = gvname(n);
    if (cg->oktrace(tf_add_node))
        qDebug() << "add_node" << CVP(n) << N << AGID(n);

    l_items l = build_graphic(n);
    if (!l.isEmpty()) {

        lqNode* g = build_node(n, l);

        if (cg->oktrace(tf_add_node))
            qDebug() << "build_node" << N << g->boundingRect();

        using namespace configure_behaviour;
        if (option_is_on(move_Edges)) {
            g->setFlags(QGraphicsItem::ItemIsSelectable|
                        QGraphicsItem::ItemIsMovable|
                        QGraphicsItem::ItemSendsGeometryChanges);
            connect(g,  SIGNAL  (itemHasChanged(QGraphicsItem::GraphicsItemChange,QVariant)),
                        SLOT    (itemHasChanged(QGraphicsItem::GraphicsItemChange,QVariant)));
        }

        QString tooltip = QString::fromUtf8(attr_str(n, "tooltip"));
        if (!tooltip.isEmpty()) {
            tooltip.replace("\\n", "\n");
            g->setToolTip(tooltip);
        }

        if (cg->oktrace(tf_show_node_type))
            qDebug() << CVP(g) << g->type();

        if (cg->is_folded(n)) {
            QCheckBox *cb = new QCheckBox;
            cb->setChecked(true);
            QGraphicsProxyWidget *ck = addWidget(cb);
            ck->setZValue(Z_FOLD);
            ck->setPos(g->boundingRect().topLeft());
        }

        names2nodes[N] = g;
        g->setName(N);

        return g;
    }
    return 0;
}

QGraphicsItem* lqXDotScene::add_edge(Ep e)
{
     if (cg->oktrace(tf_add_edge))
        qDebug() << "add_edge" << CVP(e) << gvname(e) << AGID(e);

    l_items l = build_graphic(e);
    if (!l.isEmpty()) {
        lqEdge *g = build_edge(e, l);
        using namespace configure_behaviour;
        if (option_is_on(associate_Edges_items))
            g->setData(Edges_Items, QVariant::fromValue(e));
        return g;
    }
    return 0;
}

/** subgraphs are an important way to specify structure
 *  when prefix named with "cluster" they get a frame around
 */
void lqXDotScene::subgraphs(Gp graph, qreal off_z)
{
    QRectF bb = graph_bb(graph);

    if (cg->oktrace(tf_add_graph))
        qDebug() << "subgraph" << bb << off_z;

    l_items l;
    if (agparent(graph) == 0) {
        bbscene = bb;
        using namespace configure_behaviour;
        if (!option_is_on(no_draw_Graph_bounding_box)) {
            // add a fake frame around scene, like SvgView does
            bb.adjust(-5,-5,+5,+5);
            addRect(bb, QPen(Qt::DashLine));
        }
        /*/ workaround multiple boxes on root graph (gvFreeLayout doesn't clear them ?)
        l = build_graphic(graph, _ldraw_); */
    }
    //else
        l = build_graphic(graph);

    if (!l.isEmpty()) {
        lqGraph *ig = build_subgraph(graph, l);
        //ig->setData(agptr, QVariant::fromValue(graph));
        ig->setZValue(dz(off_z));
    }

    cg->for_subgraphs([&](Gp g) { subgraphs(g, off_z); }, graph);
}

static const char *ops[] = {"_draw_", "_ldraw_", "_hdraw_", "_tdraw_", "_hldraw_", "_htdraw_"};

/** apply XDOT attributes <b_ops> rendering to required object <obj>
 */
void lqXDotScene::perform_attrs(void* obj, int b_ops, std::function<void(const xdot_op& op)> worker) const
{
    if (cg->oktrace(tf_perform_attrs))
        qDebug() << "perform_attrs" << b_ops;

    for (size_t i = 0; i < sizeof(ops)/sizeof(ops[0]); ++i)
        if (b_ops & (1 << i)) {
            cstr a = attr_str(obj, ops[i]);
            if (a && *a) {
                if (xdot *v = parseXDot(ccstr(a))) {
                    if (cg->oktrace(tf_parseXDot))
                        qDebug() << "parseXDot" << v->cnt << ccstr(a);
                    for (int c = 0; c < v->cnt; ++c)
                        worker(v->ops[c]);
                    freeXDot(v);
                }
            }
        }
}

/** remove XDOT computed rendering attributes from object
 */
void lqXDotScene::clear_XDotAttrs(void *obj, int b_ops) {
    //qDebug() << "clear_XDotAttrs" << CVP(obj) << gvname(obj);
    auto cf = [](void *obj, const char *n) {
        //int rc = agset(obj, ccstr(n), ccstr(""));
        /*int rc = */agsafeset(obj, ccstr(n), ccstr(""), ccstr(""));
        /*if (cg->oktrace(tf_parseXDot))
            qDebug() << n << rc;*/
    };
    for (size_t i = 0; i < sizeof(ops)/sizeof(ops[0]); ++i)
        if (b_ops & (1 << i))
            cf(obj, ops[i]);
    cf(obj, "pos");

    /* these don't work...
    agset(obj, ccstr("pos"), ccstr(""));
    agset(obj, ccstr("width"), ccstr(""));
    agset(obj, ccstr("height"), ccstr(""));
    */
}

/** compute *only* the bounding rect
 */
QRectF lqXDotScene::bb_rect(void *obj, int ops)
{
    QRectF bb;
    const char* fontname = 0;
    qreal fontsize = 0;
    QFont font;

    auto poly_rect = [](const t_poly &p) {
        Q_ASSERT(p.size() > 0);

        QPointF tl = p[0], br = p[0];
        for (int i = 1; i < p.size(); ++i) {
            QPointF t = p[i];
            using namespace std;
            tl.setX(min(tl.x(), t.x()));
            br.setX(max(br.x(), t.x()));
            tl.setY(min(tl.y(), t.y()));
            br.setY(max(br.y(), t.y()));
        }

        return QRectF(tl, br);
    };

    perform_attrs(obj, ops, [&](const xdot_op& op) {
        switch (op.kind) {
        case xd_filled_ellipse:
        case xd_unfilled_ellipse: {
            bb = bb.united(rect_spec(op.u.ellipse));
        }   break;
        case xd_filled_polygon:
        case xd_unfilled_polygon: {
            bb = bb.united(poly_rect(poly_spec(op.u.polygon)));
        }   break;
        case xd_filled_bezier:
        case xd_unfilled_bezier: {
            bb = bb.united(poly_rect(poly_spec(op.u.bezier)));
        }   break;
        case xd_polyline: {
            bb = bb.united(poly_rect(poly_spec(op.u.polyline)));
        }   break;
        case xd_text: {
            const xdot_text &xt = op.u.text;
            QString text(QString::fromUtf8(xt.text));
            QString family = font_spec(fontname);
            font.setFamily(family);
            font.setPixelSize(fontsize);
            QFontMetricsF fm(font);
            QRectF tbr = fm.boundingRect(text);
            bb = bb.united(tbr);
        }   break;
        case xd_font:
            fontsize = op.u.font.size;
            fontname = op.u.font.name;
            break;
        case xd_fill_color:
        case xd_pen_color:
        case xd_style:
        case xd_grad_fill_color:
        case xd_grad_pen_color:
            break;
        case xd_image: {
            Q_ASSERT(false);
        }   break;
        case xd_fontchar: {
        }   break;
        }
    });

    return bb;
}

/** this is the core of the class
  * it turns out it's *simple* to interpret xdot output commands
  */
lqXDotScene::l_items lqXDotScene::build_graphic(void *obj, int b_ops)
{
    l_items l;

    auto _bezier = [this](const xdot_polyline& l) {
        t_poly pts = poly_spec(l);
        QPainterPath path;
        path.moveTo(pts[0]);
        for (int i = 1; i < pts.size() - 1; i += 3)
            path.cubicTo(pts[i], pts[i+1], pts[i+2]);
        return addPath(path);
    };

    auto _gradient = [this](const xdot_color &dc) {
        if (dc.type == xd_linear) {
            const xdot_linear_grad &l = dc.u.ling;
            QLinearGradient grad(QPointF(l.x0, cy(l.y0)), QPointF(l.x1, cy(l.y1)));
            for (int s = 0; s < l.n_stops; ++s)
                grad.setColorAt(l.stops[s].frac, parse_color(l.stops[s].color, truecolor()));
            return QBrush(grad);
        }
        else {
            Q_ASSERT(dc.type == xd_radial);
            const xdot_radial_grad &r = dc.u.ring;
            QRadialGradient grad(QPointF(r.x1, cy(r.y1)), r.r1, QPointF(r.x0, cy(r.y0)), r.r0);
            for (int s = 0; s < r.n_stops; ++s)
                grad.setColorAt(r.stops[s].frac, parse_color(r.stops[s].color, truecolor()));
            return QBrush(grad);
        }
    };

    // keep state while scanning draw instructions
    QBrush brush;
    QPen pen;
    const char* fontname = 0;
    const char* currcolor = 0;
    qreal fontsize = 0;
    QFont font;

    enum {
        dashed  = 1<<0,
        dotted  = 1<<1,
        solid   = 1<<2,
        invis   = 1<<3,
        bold    = 1<<4
    };
    int b_style = 0;

    perform_attrs(obj, b_ops, [&](const xdot_op &op) {

        if (cg->oktrace(tf_perform_attrs_xdot_op))
            qDebug() << "perform_attrs" << b_ops << op.kind;

        switch (op.kind) {
        case xd_filled_ellipse: {
            auto p = addEllipse(rect_spec(op.u.ellipse));
            p->setBrush(brush);
            p->setPen(pen);
            l << p;
        }   break;

        case xd_unfilled_ellipse: {
            auto p = addEllipse(rect_spec(op.u.ellipse));
            p->setPen(pen);
            l << p;
        }   break;

        case xd_filled_polygon: {
            auto p = addPolygon(poly_spec(op.u.polygon));
            p->setBrush(brush);
            p->setPen(pen);
            l << p;
        }   break;

        case xd_unfilled_polygon: {
            auto p = addPolygon(poly_spec(op.u.polygon));
            p->setPen(pen);
            l << p;
        }   break;

        case xd_filled_bezier: {
            auto p = _bezier(op.u.bezier);
            p->setBrush(brush);
            p->setPen(pen);
            l << p;
        }   break;

        case xd_unfilled_bezier: {
            auto p = _bezier(op.u.bezier);
            p->setPen(pen);
            l << p;
        }   break;

        case xd_polyline: {
            auto p = addPolygon(poly_spec(op.u.polyline));
            p->setPen(pen);
            l << p;
        }   break;

        case xd_text: {
            const xdot_text &xt = op.u.text;

            QString text(QString::fromUtf8(xt.text));
            QString family(font_spec(fontname));

            /* can't solve font properties
             * there could be a bug in xdot...
            font.setFamily(family);

            // set the pixel size of the font.
            font.setPixelSize(fontsize);
            */
            font.setFamily("FreeSerif");
            font.setPixelSize(fontsize-1);
            //font.setStyleHint(QFont::Serif);
            //font.setPixelSize(14);

            QGraphicsTextItem *t = addText(text = text.replace("\\n", "\n"), font);
            t->setDefaultTextColor(parse_color(currcolor, truecolor()));

            // this is difficult to get right
            QFontMetricsF fm(font);
            QRectF tbr = fm.boundingRect(text);

            // TBD: why 3 is required ?
            switch (xt.align) {
            case xd_left:
                t->setPos(xt.x + xt.width / 2 - 3 - tbr.width() / 2, cy(xt.y) - tbr.height());
                break;
            case xd_center:
                t->setPos(xt.x - 3 - tbr.width() / 2, cy(xt.y) - tbr.height());
                break;
            case xd_right:
                Q_ASSERT(0);
                break;
            }
            l << t;
        }   break;

        case xd_fill_color:
            brush = QBrush(parse_color(currcolor = op.u.color, truecolor()));
            break;

        case xd_pen_color:
            pen = QPen(parse_color(currcolor = op.u.color, truecolor()));
            if ((b_style & (dashed|dotted)) == (dashed|dotted))
                pen.setStyle(Qt::DashDotLine);
            else if (b_style & dashed)
                pen.setStyle(Qt::DashLine);
            else if (b_style & dotted)
                pen.setStyle(Qt::DotLine);
            if (b_style & bold)
                pen.setWidth(3);
            break;

        case xd_font:
            fontsize = op.u.font.size;
            fontname = op.u.font.name;
            break;

        case xd_style: {
            /** from http://www.graphviz.org/content/attrs#dstyle
                At present, the recognized style names are "dashed", "dotted", "solid", "invis" and "bold" for nodes and edges,
                "tapered" for edges only, and "filled", "striped", "wedged", "diagonals" and "rounded" for nodes only.
                The styles "filled", "striped" and "rounded" are recognized for clusters.
                The style "radial" is recognized for nodes, clusters and graphs, and indicates a radial-style gradient fill if applicable.
            */
            //enum N_style { dashed, dotted, solid, invis, bold, filled, striped, wedged, diagonals, rounded };
            //enum E_style { dashed, dotted, solid, invis, bold, tapered };
            //enum G_style { filled, striped, rounded };
            foreach (QString s, QString(op.u.style).split(','))
                if (s == "dashed")  b_style |= dashed; else
                if (s == "dotted")  b_style |= dotted; else
                if (s == "bold")    b_style |= bold; else
                if (s != "solid")   {
                    if (cg->oktrace(tf_perform_attrs_xdot_op))
                        qDebug() << "unhandled style" << op.u.style;
                }
            }
            break;

        case xd_image: {
            const xdot_image &xi = op.u.image;
            QPixmap i;
            if (i.load(xi.name) || i.load(imagepath_ + xi.name)) {
                QGraphicsPixmapItem *pm = new QGraphicsPixmapItem(i);
                pm->setPos(xi.pos.x, cy(xi.pos.y + xi.pos.h));
                l << pm;
            }
            else
                qDebug() << "cannot load image" << xi.name;
        }   break;

        case xd_grad_fill_color:
            brush = _gradient(op.u.grad_color);
            break;

        case xd_grad_pen_color:
            pen = QPen(_gradient(op.u.grad_color), 1);
            break;

        case xd_fontchar: {
            int fontchar = op.u.fontchar;
            enum {BOLD, ITALIC, UNDERLINE, SUPERSCRIPT, SUBSCRIPT, STRIKE_THROUGH};
            if (fontchar & (1 << BOLD))
                font.setBold(true);
           if (fontchar & (1 << ITALIC))
                font.setItalic(true);
            if (fontchar & (1 << UNDERLINE))
                font.setUnderline(true);

            if (fontchar & (1 << SUPERSCRIPT))
                Q_ASSERT(false);
            if (fontchar & (1 << SUBSCRIPT))
                Q_ASSERT(false);

            if (fontchar & (1 << STRIKE_THROUGH))
                font.setStrikeOut(true);
        }   break;
        }
    });

    return l;
}

/** debugging utility, dump graph structure to trace
 */
void lqXDotScene::dump(QString m) const { cg->dump(m); }

class changeScene : public lqAniMachine::cleanUpState {
public:
    changeScene(lqXDotScene *s, lqXDotView *v, QPointF p, QStateMachine *m) :
        cleanUpState(m), s(s), v(v), p(p) { }
protected:
    lqXDotScene *s;
    lqXDotView *v;
    QPointF p;
    void onEntry(QEvent *event) {
        v->setFoldedScene(s, p);
        cleanUpState::onEntry(event);
    }
};

/** compute scene animation to get a visible node <i> folded/unfolded
 */
lqXDotScene* lqXDotScene::fold(lqNode* i, lqXDotView *qv)
{
    Np n = it_node(i);   // n is undergoing folding
    if (!n)
        return 0;

    QString N = gvname(n);

    // mandatory to recompute...
    { bool rc = cg->freeLayout(); Q_ASSERT(rc); Q_UNUSED(rc); }

    bool is_folded = cg->is_folded(n);
    if (is_folded)
        cg->unfold(n);
    else {
        if (agfstout(*cg, n) == 0)
            return 0;
        cg->fold(n);
    }

    if (!cg->repeatOperations())
        return 0;

    auto xs = new lqXDotScene(cg);
    xs->build();

    auto am = new lqAniMachine;

    auto lo = new lqLogger(this, SLOT(msg(QString)), am);
    lo->print(am, SIGNAL(finished()), "machine finished");

    /*
    QRectF R = i->boundingRect();
    QRectF R1 = xs->names2nodes[N]->boundingRect();
    QPointF P = R.center() - R1.center();

    for (name2node::const_iterator ib = names2nodes.begin(); ib != names2nodes.end(); ++ib) {
        lqNode *i1 = ib.value();
        if (i1 != i) {
            QRectF Q = i1->boundingRect();
            name2node::const_iterator ia = xs->names2nodes.find(ib.key());
            if (ia == xs->names2nodes.end()) {
                // removed
                QPointF X_p = R.center() - Q.center() + P;
                if (!X_p.isNull()) {
                    am->animateTargetProperty(i1, "pos", X_p);
                    am->animateTargetProperty(i1, "opacity", 0);
                }
            } else {
                // move to new position
                QRectF Q1 = ia.value()->boundingRect();
                QPointF X_p = R1.center() - Q1.center() + P;
                if (!X_p.isNull())
                    am->animateTargetProperty(i1, "pos", X_p);
            }
        }
    }

    am->run(new changeScene(xs, qv, P, am));
    */

    //QRectF R = i->boundingRect();
    QRectF R1 = xs->names2nodes[N]->boundingRect();
    //QPointF P = R1.center() - R.center();

    foreach(auto i, items()) {
        if (i->type() == QGraphicsItemGroup::Type)
            i->hide();
    }

    if (!is_folded) {
        Q_ASSERT(names2nodes.count() > xs->names2nodes.count());
        for (name2node::const_iterator it = names2nodes.begin(); it != names2nodes.end(); ++it) {
            QPointF X_p;
            name2node::const_iterator ix = xs->names2nodes.find(it.key());
            if (ix == xs->names2nodes.end()) {
                // get folded - removed
                X_p = R1.center() - it.value()->boundingRect().center();
                am->animateTargetProperty(it.value(), "opacity", 0);
            } else {
                // move to new position
                QRectF Q = it.value()->boundingRect();
                QRectF Q1 = ix.value()->boundingRect();
                X_p = Q1.center() - Q.center();
            }
            if (!X_p.isNull())
                am->animateTargetProperty(it.value(), "pos", X_p);
        }
    }
    else {
        Q_ASSERT(names2nodes.count() < xs->names2nodes.count());
        for (name2node::const_iterator ix = xs->names2nodes.begin(); ix != xs->names2nodes.end(); ++ix) {
            QPointF X_p;
            name2node::const_iterator it = names2nodes.find(ix.key());
            if (it == names2nodes.end()) {
                // get folded - removed
                //X_p = P;
                //am->animateTargetProperty(it.value(), "opacity", 0);
            } else {
                // move to new position
                QRectF Q = it.value()->boundingRect();
                QRectF Q1 = ix.value()->boundingRect();
                X_p = Q1.center() - Q.center();
                am->animateTargetProperty(it.value(), "pos", X_p);
            }
        }
    }
    am->run(new changeScene(xs, qv, QPointF(), am));
    am->start();

    return xs;
}

/** really a logging utility
 */
void lqXDotScene::msg(QString m) {
    qDebug() << QTime::currentTime() << m;
}

/** parse the bounding rect attribute on <graph> to QRectF
 */
QRectF lqXDotScene::graph_bb(Gp graph)
{
    QRectF bb;
    QString bbs = attr_str(graph, "bb");
    if (bbs.length()) {
        qreal left, top, width, height;
        QChar s;
        if ((QTextStream(&bbs) >> left >> s >> top >> s >> width >> s >> height).status() == QTextStream::Ok) {
            bb = QRectF(left, top, width, height);
        }
        else
            msg(tr("invalid bb on %1").arg(gvname(graph)));
    }
    return bb;
}

/** just redo changed objects
 */
void lqXDotScene::redo_objects(QList<void*> objects)
{
    if (cg->oktrace(tf_redo_objects))
        qDebug() << "redo_objects" << objects.count();

    foreach(CVP o, objects)
        if (auto E = add_edge(Ep(o))) {
            static qreal ez = 10;
            E->setZValue(dz(ez));
        }
}

/** WIP attempt to preserve layout and recomputing edges only
 */
void lqXDotScene::moveEdges(lqNode *nodeMoving, QPointF deltaPos)
{
    qDebug() << "moveEdges" << nodeMoving->name() << deltaPos;

    cg->clearXDotAttrs();
    QString n = nodeMoving->name();

    Np N = to_node(nodeMoving);
    if (N) {
        qDebug() << gvname(N);
        if (agsafeset(N, ccstr("pos"), qcstr(QString("%1,%2").arg(deltaPos.x(), deltaPos.y())), ccstr("0,0")))
            qDebug() << "failed";
    }

    #define F else qDebug() << __LINE__;

    QString fdot = "/tmp/x1.dot", fneato = "/tmp/x1.neato";
    if (gvRenderFilename(*cg, *cg, "dot", qcstr(fdot)) == 0) {
        QFile src(fdot);
        if (src.open(src.ReadOnly)) {
            QString d = make_nop().transform(QTextStream(&src).readAll());
            if (!d.isEmpty()) {
                QString pn = "\t\"" + n + "\"\t";
                int q = d.indexOf(pn);
                if (q == -1)
                    pn = "\t" + n + "\t";
                q = d.indexOf(pn);
                if (q > 0) {
                    int y = d.indexOf("pos=\"", q + pn.length());
                    if (y > 0) {
                        int z = d.indexOf("\",\n", y + 5);
                        if (z > 0) {
                            QString cp = d.mid(y + 5, z - y - 5);
                            QStringList l = cp.split(",");
                            if (l.count() == 2) {
                                bool ok = false;
                                float XP = l[0].toFloat(&ok);   qDebug() << cp << l[0] << l[1] << ok;
                                if (ok) {
                                    float YP = l[1].toFloat(&ok);
                                    if (ok) {
                                        QString g = QString("%1,%2").arg(XP + deltaPos.x()).arg(YP - deltaPos.y());
                                        QString D = d.left(y+5) + g + d.mid(z);
                                        QFile dst(fneato);
                                        if (dst.open(dst.WriteOnly)) {
                                            {QTextStream(&dst) << D;}
                                            emit reload_layout(fneato);
                                        } F
                                    } F
                                } F
                            } F
                        } F
                    } F
                } F
            } F
        } F
    } F

    #undef F

#if 0
    /*int rf = gvRenderFilename(*cg, *cg, ccstr("plain"), ccstr("/tmp/x.plain"));
    qDebug() << rf;

    rf = gvRenderFilename(*cg, *cg, ccstr("xdot"), ccstr("/tmp/x.xdot"));
    qDebug() << rf;
    */

    gvRenderFilename(*cg, *cg, "dot", "/tmp/x.dot");
    cg->clearXDotAttrs();
    gvRenderFilename(*cg, *cg, "dot", "/tmp/x1.dot");

    Np n = to_node(nodeMoving);
    QString ss(agget(n, ccstr("pos")));
    int ps = ss.indexOf(",");
    Q_ASSERT(ps > 0);
    qreal   x = ss.left(ps).toFloat(),
            y = ss.mid(ps + 1).toFloat();
    qDebug() << ss << x << y;

    //x -= delta.x();
    y -= deltaPos.y();

    QMap<Ep, QPair<Np, Np> > l_changed;
    gvFreeLayout(*cg, *cg);

    agsafeset(Gp(*cg), ccstr("splines"), ccstr("true"), ccstr(""));
    qDebug() << agget(n, ccstr("pos"));
    agset(n, ccstr("pos"), QString("%1,%2!").arg(x).arg(y).toUtf8().data());

    auto etrace = [&](Ep e) {
        if (!l_changed.contains(e))
            l_changed[e] = qMakePair(agtail(e), aghead(e));
    };
    cg->for_edges_in(n, etrace);
    cg->for_edges_out(n, etrace);

    foreach(auto e, l_changed.keys())
        agdeledge(*cg, e);

    QList<void*> l_new;
    foreach(auto e, l_changed.keys())
        l_new << agedge(*cg, l_changed[e].first, l_changed[e].second, 0, 1);

    gvLayout(*cg, *cg, "nop2");
    gvRender(*cg, *cg, "xdot", 0);
    if (cg->oktrace(tf_layout))
        gvRenderFilename(*cg, *cg, "xdot", "/tmp/x.nop2");

    //redo_objects(l_new);
    clear();
    build();

    qDebug() << agget(n, ccstr("pos"));
#endif
}

void lqXDotScene::itemHasChanged(QGraphicsItem::GraphicsItemChange c, QVariant v)
{
    if (c == QGraphicsItem::ItemPositionChange) {
        if (nodeMoving && nodeMoving == qobject_cast<lqNode*>(sender())) {
            if (nodeOldPos == nodeNewPos)
                nodeOldPos = v.toPointF();
            else
                nodeNewPos = v.toPointF();
        }
    }

    if (c == QGraphicsItem::ItemSelectedChange) {
        qDebug() << "ItemSelectedChange";
        if (auto n = qobject_cast<lqNode*>(sender())) {
            if (v.toBool()) {
                nodeMoving = n;
                nodeOldPos = nodeNewPos = QPointF();// = v.toPointF();
            }
            else
                nodeMoving = 0;
        }
    }
}

/** this doesn't work, family defaulted to FreeSerif */
QString lqXDotScene::font_spec(cstr fontname) {
    QString family(fontname);
    int sep = family.indexOf('-');
    if (sep > 0)
        family = QString("%1 [%2]").arg(family.left(sep), family.mid(sep + 1));
    return family;
}

void lqXDotScene::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
    QGraphicsScene::mousePressEvent(event);
}

void lqXDotScene::mouseMoveEvent(QGraphicsSceneMouseEvent *event)
{
    QGraphicsScene::mouseMoveEvent(event);
}

void lqXDotScene::mouseReleaseEvent(QGraphicsSceneMouseEvent *event)
{
    if (nodeMoving) {
        if (nodeNewPos != nodeOldPos)
            moveEdges(nodeMoving, nodeNewPos - nodeOldPos);
        nodeMoving = 0;
    }
    QGraphicsScene::mouseReleaseEvent(event);
}
