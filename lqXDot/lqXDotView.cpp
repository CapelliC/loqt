/*
    lqXDot       : interfacing Qt and Graphviz library

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014 Carlo Capelli

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

#include "lqXDotView.h"
#include "lqAobj.h"

#include <qmath.h>
#include <QDebug>
#include <QKeyEvent>
#include <QWheelEvent>
#include <QMessageBox>
#include <QGraphicsSceneMouseEvent>
#include <QTextBlockFormat>
#include <QTextCursor>
#include <QDir>
#include <QMenu>
#include <QTimer>

/** actual constructor, make an empty view
 */
lqXDotView::lqXDotView(QWidget* parent)
    : QGraphicsView(parent)
{
    setup();
}

/** required by scripting
 */
lqXDotView::lqXDotView(const lqXDotView &v)
    : QGraphicsView()
{
    Q_UNUSED(v)
    setup();
}

/** set default settings, for high quality display
 */
void lqXDotView::setup()
{
    setTruecolor(false);
    setFoldNodes(true);

    cg = new lqContextGraph(this);

    setRenderHint(QPainter::Antialiasing);
    setRenderHint(QPainter::TextAntialiasing);
    setTransformationAnchor(AnchorUnderMouse);
    setResizeAnchor(AnchorUnderMouse);
    setDragMode(ScrollHandDrag);
}

lqXDotView::~lqXDotView()
{
    delete cg;
}

/** parse the .gv source, render in default view
  */
bool lqXDotView::render_file(QString source, QString algo)
{
    return cg->run_with_error_report([&]() {
        QString err;
        if (FILE* fp = fopen(source.toUtf8().constData(), "r")) {
            if (cg->parse(fp)) {
                setLayoutKind(algo);
                if (render_layout(err))
                    render_graph();
            } else
                err = tr("agread() failed: %1").arg(source);
            fclose(fp);
        }
        else
            err = tr("cannot open file: %1").arg(source);
        return err;
    });
}

/** render the script with given layout
  */
bool lqXDotView::render_script(QString script, QString algo)
{
    return cg->run_with_error_report([&]() {
        QString err;
        if (cg->parse(script)) {
            setLayoutKind(algo);
            if (render_layout(err))
                render_graph();
        } else
            err = tr("amemgread() failed: %1");
        return err;
    });
}

/** apply scene translation to a built GV graph
  */
void lqXDotView::render_graph()
{
    QString tc = attr_str(Gp(*cg), "truecolor");
    truecolor_ = tc == "yes" || tc == "true";

    imagepath_ = attr_str(Gp(*cg), "imagepath");
    if (!imagepath_.isEmpty()) {
        QDir cd(imagepath_);
        if (cd.exists())
            QDir::setCurrent(cd.path());
    }

    setScene(new lqXDotScene(cg));
}

/** very simple keyboard interaction
 */
void lqXDotView::keyPressEvent(QKeyEvent* event)
{
    switch (event->key()) {
    case Qt::Key_Plus:
        scale_view(1.2);
        break;
    case Qt::Key_Minus:
        scale_view(1.0 / 1.2);
        break;

    case Qt::Key_Asterisk:
        rotate(10.0);
        break;
    case Qt::Key_Slash:
        rotate(-10.0);
        break;

    default:
        QGraphicsView::keyPressEvent(event);
    }
}

/** very simple mouse interaction
 */
void lqXDotView::wheelEvent(QWheelEvent* event)
{
    qreal factor = qPow(1.2, event->delta() / 240.0);
    scale(factor, factor);
    event->accept();
}

/** create a menu of commands on selected node
 */
void lqXDotView::contextMenuEvent(QContextMenuEvent *event)
{
    if (QGraphicsItem *item = itemAt(event->pos())) {
        qDebug() << CVP(item) << item->type();
        if (Np n = scene()->it_node(item)) {
            qDebug() << "menu on " << gvname(n);
            QMenu menu(this);
            QString s;
            if (cg->is_folded(n))
                s = tr("&Unfold");
            else
                s = tr("&Fold");
            QAction *a = menu.addAction(s, this, SLOT(toggleFolding()));
            lqNode *N = ancestor<lqNode>(item);
            a->setData(QVariant::fromValue(N));
            menu.exec(event->globalPos());
        }
    }
}

/** apply Qt scaling as specified
 */
void lqXDotView::scale_view(qreal scaleFactor)
{
    qreal f = sqrt(matrix().det());

    if (scaleFactor * f > 8.0)
        scaleFactor = 8.0 / f;
    if (scaleFactor * f < 0.1)
        scaleFactor = 0.1 / f;

    scale(scaleFactor, scaleFactor);
}

/** given layout, issue xdot rendering
 */
bool lqXDotView::render_layout(QString &err)
{
    if (cg->layout(layoutKind_)) {
        if (cg->render("xdot"))
            return true;
        err = tr("gvRender() xdot failed on %1").arg(layoutKind_);
    }
    else
        err = tr("gvLayout(%1) failed").arg(layoutKind_);
    return false;
}

/** script interface, allows context and graph to be created externally
 */
void lqXDotView::show_context_graph_layout(GVC_t* context, Agraph_t *graph, QString layout)
{
    cg = new lqContextGraph(context, graph, this);

    QGraphicsView::show();

    char *LC = setlocale(LC_ALL, "C");
    setLayoutKind(layout);
    QString err;
    if (render_layout(err)) {
        render_graph();
        cg->freeLayout();
    }
    else
        critical(err);

    setlocale(LC_ALL, LC);
}

GVC_t* lqXDotView::getContext() const
{
    return Cp(*cg);
}
Agraph_t* lqXDotView::getGraph() const
{
    return Gp(*cg);
}

/** perform graph management as required to fold nodes
 */
void lqXDotView::toggleFolding()
{
    if (foldNodes()) {
        lqNode *i = qobject_cast<QAction*>(sender())->data().value<lqNode *>();
        lqXDotScene *s = scene()->fold(i, this);
        if (s) {
        }
    }
}
void lqXDotView::setFoldedScene(lqXDotScene* s, QPointF p) {
    Q_UNUSED(p)
    setRenderHint(QPainter::Antialiasing);
    setRenderHint(QPainter::TextAntialiasing);
    //clear();
    setScene(s);
    //translate(p.x(), p.y());
}
