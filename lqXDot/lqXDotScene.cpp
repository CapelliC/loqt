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

#include "lqXDotScene.h"

#include <QTime>
#include <QDebug>
#include <QFinalState>
#include <QFontMetrics>
#include <QSignalMapper>
#include <QStateMachine>
#include <QPropertyAnimation>
#include <QAbstractTransition>
#include <QParallelAnimationGroup>

inline qreal dz(qreal &v) { v += .0001; return v; }

lqXDotScene::lqXDotScene(lqContextGraph *cg) : cg(cg),
    truecolor_()
{
    build();
}

void lqXDotScene::build()
{
    QString tc = attr_str(Gp(*cg), "truecolor");
    truecolor_ = tc == "yes" || tc == "true";

    imagepath_ = attr_str(Gp(*cg), "imagepath");

    subgraphs(Gp(*cg), 1);

    qreal z_node = 2, z_edge = 3;

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
                return charv[uchar(color[p].toAscii())] * 16 + charv[uchar(color[p+1].toAscii())];
            };
            int R = chex(1),
                G = chex(3),
                B = chex(5),
                A = chex(7);
            r.setRgb(R, G, B, A);
        }
    return r;
}

typedef const void* CVP;

QGraphicsItem* lqXDotScene::add_node(Np n)
{
    qDebug() << "add_node" << CVP(n) << gvname(n);

    l_items l = build_graphic(n);
    if (!l.isEmpty()) {
        lqNode* g = new lqNode(this, l);

        g->setData(agptr, QVariant::fromValue(n));
        g->setFlag(g->ItemIsSelectable);

        QString tooltip = QString::fromUtf8(attr_str(n, "tooltip"));
        if (!tooltip.isEmpty()) {
            tooltip.replace("\\n", "\n");
            g->setToolTip(tooltip);
        }

        return g;
    }
    return 0;
}

QGraphicsItem* lqXDotScene::add_edge(Ep e)
{
    l_items l = build_graphic(e);
    if (!l.isEmpty()) {
        QGraphicsItemGroup* g = createItemGroup(l);
        g->setData(agptr, QVariant::fromValue(e));
        return g;
    }
    return 0;
}

// subgraphs are an important way to specify structure
// when prefix named with "cluster" they get a frame around
//
void lqXDotScene::subgraphs(Gp graph, qreal off_z)
{
    QRectF bb = graph_bb(graph);

    l_items l;
    if (agparent(graph) == 0) {
        bbscene = bb;
        // add a fake frame around scene, like SvgView does
        bb.adjust(-5,-5,+5,+5);
        addRect(bb, QPen(Qt::DashLine));
        /*/ workaround multiple boxes on root graph (gvFreeLayout doesn't clear them ?)
        l = build_graphic(graph, _ldraw_); */
    }
    //else
        l = build_graphic(graph);

    if (!l.isEmpty()) {
        QGraphicsItem *ig = createItemGroup(l);
        ig->setData(agptr, QVariant::fromValue(graph));
        ig->setZValue(dz(off_z));
    }

    cg->for_subgraphs(graph, [&](Gp g) { subgraphs(g, off_z); });
}

void lqXDotScene::perform_attrs(void* obj, int b_ops, std::function<void(const xdot_op& op)> worker)
{
    static const char *ops[] = {"_draw_", "_ldraw_", "_hdraw_", "_tdraw_", "_hldraw_", "_htdraw_"};
    for (size_t i = 0; i < sizeof(ops)/sizeof(ops[0]); ++i)
        if (b_ops & (1 << i)) {
            cstr a = attr_str(obj, ops[i]);
            if (a && *a) {
                if (xdot *v = parseXDot(ccstr(a))) {
                    for (int c = 0; c < v->cnt; ++c)
                        worker(v->ops[c]);
                    freeXDot(v);
                }
            }
        }
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

    perform_attrs(obj, b_ops, [&](const xdot_op &op) {
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
            l << p;
        }   break;

        case xd_text: {
            const xdot_text &xt = op.u.text;

            QString text(QString::fromUtf8(xt.text));
            QString family(font_spec(fontname));

            font.setFamily(family);

            // set the pixel size of the font.
            font.setPixelSize(fontsize);

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
            break;

        case xd_font:
            fontsize = op.u.font.size;
            fontname = op.u.font.name;
            break;

        case xd_style:
            /** even without implementing this I get good results ! */
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
void lqXDotScene::dump(QString m) const {
    qDebug() << m;
    cg->depth_first([&](Gp t) { qDebug() << "graph" << gvname(t) << CVP(find_graph(t)); });
    qDebug() << "nodes";
    cg->for_nodes([&](Np n) {
        qDebug() << "node" << gvname(n) << CVP(find_node(n));
        cg->for_edges_out(n, [&](Ep e) {
            qDebug() << "edge" << gvname(e) << CVP(find_edge(e)) << "to" << gvname(e->node) << CVP(find_node(e->node));
        });
    });
}

class cleanUpState : public QFinalState {
public:
    cleanUpState(QStateMachine *m) : QFinalState(m) {}
protected:
    void onEntry(QEvent *event) {
        qDebug() << CVP(machine()) << "machine()->deleteLater()";
        QFinalState::onEntry(event);
        machine()->deleteLater();
    }
};

/** perform scene manipulation to get a node folded
 */
bool lqXDotScene::f_old(lqNode* i)
{
    QStateMachine *m = new QStateMachine;

    Np n = it_node(i);   // n is undergoing folding
    Q_ASSERT(n);

    dump("before");

    QSignalMapper *sm = new QSignalMapper(m);
    connect(sm, SIGNAL(mapped(QString)), this, SLOT(msg(QString)));

    auto pmsg = [&](QObject* emitter, const char* sig, QString m) {
        connect(emitter, sig, sm, SLOT(map()));
        sm->setMapping(emitter, m);
    };

    /*
    int xc = agsafeset(n, ccstr("shape"), ccstr("folder"), ccstr("ellipse"));
    Q_ASSERT(xc == 0);
    */

    edges edel;
    nodes ndel;

    // compute animation
    QState *source = new QState(m);
    m->setInitialState(source);

    //foldedState *target = new foldedState(m);
    auto target = new QState(m);

    QAbstractTransition *s2t = source->addTransition(target);

    QParallelAnimationGroup *all = new QParallelAnimationGroup;
    s2t->addAnimation(all);

    // delete machine after animation
    target->addTransition(all, SIGNAL(finished()), new cleanUpState(m));
    pmsg(m, SIGNAL(finished()), "machine finished");

    // to recompute...
    if (!cg->freeLayout())
        return false;

    // structural changes
    cg->for_edges_out(n, [&](Ep e) {
        Np h = e->node;

        // move edges from hidden to source
        edel << e;
        cg->for_edges_out(h, [&](Ep d) {
            //target->edges_moved << find_edge(d);
            find_edge(d)->hide();
            //target->e_cloned << cg->clone(d, );
        });

        if (!ndel.contains(h)) {    // multiple edges ?
            // remove node
            ndel << h;

            //target->nodes_hidden << find_node(h);
            //target->n_cloned << cg->clone(h);
        }
    });

    foreach(auto x, edel)
        agdeledge(*cg, x);
    foreach(auto x, ndel)
        agdelnode(*cg, x);

    if (cg->repeatOperations()) {

        // used to offset y coords in scene
        bbscene = graph_bb(*cg);

        QRectF N = bb_rect(n);
        QPointF Nd = find_node(n)->boundingRect().center() - N.center();

        cg->for_nodes([&](Np x) {
            Q_ASSERT(!ndel.contains(x));
            lqNode *X = find_node(x);
            QRectF Q = X->boundingRect();
            QPointF X_p;
            if (x == n) {
                //X_p = N.center() - Q.center();
            } else {
                QRectF X1 = bb_rect(x);
                X_p = X1.center() - Q.center() + Nd;
            }
            if (!X_p.isNull()) {
                target->assignProperty(X, "pos", X_p);
                all->addAnimation(new QPropertyAnimation(X, "pos"));
                cg->for_edges_out(n, [&](Ep e){ find_edge(e)->hide(); });
            }
        });

        foreach(auto x, ndel) {
            lqNode *X = find_node(x);
            QRectF Q = X->boundingRect();
            QPointF X_p = N.center() - Q.center() + Nd;

            target->assignProperty(X, "pos", X_p);
            all->addAnimation(new QPropertyAnimation(X, "pos"));

            target->assignProperty(X, "opacity", 0);
            all->addAnimation(new QPropertyAnimation(X, "opacity"));
        }
    }

    for (int i = 0; i < all->animationCount(); ++i)
        qobject_cast<QPropertyAnimation*>(all->animationAt(i))->setDuration(1000);

    m->start();
    return true;
}

/** perform scene manipulation to get a node folded
 */
bool lqXDotScene::fold(lqNode* i)
{
    Np n = it_node(i);   // n is undergoing folding
    Q_ASSERT(n);

    dump("before");
    if (cg->is_folded(n))
        cg->unfold(n);
    else
        cg->fold(n);
    /*
    if (cg->cloned.contains(n)) {
        // unfold
        cg->restore(n);
    }
    else {
        edges edel;
        nodes ndel;

        Np N = cg->clone(n);

        // structural changes
        cg->for_edges_out(n, [&](Ep e) {
            Np h = e->node;

            // move edges from hidden to source
            edel << e;
            cg->clone(n, e);

            //Ep E = agedge(agraphof(N), N, H);
            //Ep E = cg->clone(e);

            if (!ndel.contains(h)) {    // multiple edges ?
                // remove node
                ndel << h;
            }
        });

        // mandatory to recompute...
        if (!cg->freeLayout())
            return false;

        foreach(auto x, edel)
            agdeledge(*cg, x);
        foreach(auto x, ndel)
            agdelnode(*cg, x);

        int xc = agsafeset(n, ccstr("shape"), ccstr("folder"), ccstr("ellipse"));
        Q_ASSERT(xc == 0);
    }
    */

    if (cg->repeatOperations()) {
        clear();
        build();
        return true;
    }

    return false;
}

void lqXDotScene::msg(QString m) {
    qDebug() << QTime::currentTime() << m;
}

QRectF lqXDotScene::graph_bb(Gp graph)
{
    QRectF bb;
    QString bbs = attr_str(graph, "bb");
    if (bbs.length()) {
        qreal left, top, width, height;
        QChar s;
        if ((QTextStream(&bbs) >> left >> s >> top >> s >> width >> s >> height).status() == QTextStream::Ok)
            bb = QRectF(left, top, width, height);
        else
            msg(tr("invalid bb on %1").arg(gvname(graph)));
    }
    return bb;
}
