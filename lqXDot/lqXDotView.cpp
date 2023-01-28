/*
    lqXDot       : interfacing Qt and Graphviz library

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright @ 2023

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
#include "file2string.h"

#include <qmath.h>
#include <QDebug>
#include <QKeyEvent>
#include <QWheelEvent>
#include <QGraphicsSceneMouseEvent>
#include <QTextBlockFormat>
#include <QTextCursor>
#include <QDir>
#include <QMenu>
#include <QTimer>
#include <QFileDialog>

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

    //cg = new lqContextGraph(this);
    cg = factory_cg(this);

    setRenderHint(QPainter::Antialiasing);
    setRenderHint(QPainter::TextAntialiasing);
    setTransformationAnchor(AnchorUnderMouse);
    setResizeAnchor(AnchorUnderMouse);
    setDragMode(ScrollHandDrag);
}

lqXDotView::~lqXDotView()
{
    delete cg;
    delete exportFmt;
}

/** parse the .gv source, render in default view
 */
bool lqXDotView::render_file(QString source, QString algo)
{
    return cg->run_with_error_report([&]() {
        QString err;
        try {
            auto script = file2string(source);
            if (cg->parse(script)) {
                setLayoutKind(algo);
                if (render_layout(err))
                    render_graph();
            } else
                throw std::runtime_error("agread() failed: " + source.toStdString());
        } catch(std::exception &ex) {
            err = ex.what();
        }
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
    truecolor_ = attr_bool(Gp(*cg), "truecolor");
    imagepath_ = attr_str(Gp(*cg), "imagepath");

    if (!imagepath_.isEmpty()) {
        QDir cd(imagepath_);
        if (cd.exists())
            QDir::setCurrent(cd.path());
    }

    //setScene(new lqXDotScene(cg));
    auto s = factory_scene(cg);
    connect(s, SIGNAL(reload_layout(QString)), SLOT(reloadLayout(QString)));
    setScene(s);
    scene()->build();
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
    //qreal factor = qPow(1.2, event->pixelDelta() / 240.0);
    //scale(factor, factor);
    auto delta = event->pixelDelta();
    scale(delta.rx(), delta.ry());
    event->accept();
}

void lqXDotView::mousePressEvent(QMouseEvent *event)
{
    qDebug() << event->pos() << event->globalPosition();

    lqNode *lqit = ancestor<lqNode>(itemAt(event->pos()));
    if (lqit)
        qDebug() << lqit->boundingRect();

    foreach(auto item, items(event->pos()))
        if (QGraphicsTextItem *it = qgraphicsitem_cast<QGraphicsTextItem*>(item)) {
            if (Np np = scene()->it_node(item))
                qDebug() << it->toPlainText() << attr_qstr(np, "pos");
            break;
        }

    QGraphicsView::mousePressEvent(event);
}

/** bind item menu to Fold/Unfold
 */
void lqXDotView::addActionsItem(QMenu *menu, QGraphicsItem *item)
{
    if (Np n = scene()->it_node(item)) {
        QString s;
        if (cg->is_folded(n))
            s = tr("&Unfold");
        else
            s = tr("&Fold");
        QAction *a = menu->addAction(s, this, SLOT(toggleFolding()));
        lqNode *N = ancestor<lqNode>(item);
        a->setData(QVariant::fromValue(N));
    }
}

/** bind scene menu to 'Export As...'
 */
void lqXDotView::addActionsScene(QMenu *menu)
{
    delete exportFmt;
    exportFmt = new QSignalMapper;
    connect(exportFmt, SIGNAL(mapped(QString)), SLOT(exportAs(QString)));

    QMenu *e = menu->addMenu(tr("&Export As..."));
    foreach (auto fmt, QString("dot svg pdf png jpg").split(' ')) {
        QAction *a = e->addAction(fmt);
        connect(a, SIGNAL(triggered()), exportFmt, SLOT(map()));
        exportFmt->setMapping(a, fmt);
    }
}

/** create a menu of commands on selected node
 */
void lqXDotView::contextMenuEvent(QContextMenuEvent *event)
{
    QMenu menu(this);
    if (QGraphicsItem *item = itemAt(event->pos())) {
        addActionsItem(&menu, item);
        if (!menu.isEmpty())
            menu.addSeparator();
    }
    addActionsScene(&menu);
    menu.exec(event->globalPos());
}

/** apply Qt scaling as specified
 */
void lqXDotView::scale_view(qreal scaleFactor)
{
    qreal f = sqrt(transform().determinant());

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
    //cg = new lqContextGraph(context, graph, this);
    cg = factory_cg(context, graph, this);

    QGraphicsView::show();

    char *LC = setlocale(LC_ALL, "C");
    setLayoutKind(layout);

    QString err;
    if (render_layout(err)) {
        render_graph();
        //cg->freeLayout();
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

void lqXDotView::exportAs(QString fmt)
{
    QFileDialog fd(this);
    fd.setAcceptMode(fd.AcceptSave);
    fd.setDefaultSuffix(fmt);
    if (!lastExportDir.isEmpty())
        fd.setDirectory(lastExportDir);
    if (fd.exec()) {
        cg->clearXDotAttrs();
        QString n = fd.selectedFiles()[0];
        qDebug() << "exporting to" << n;
        lastExportDir = fd.directory().path();
        cg->run_with_error_report([&]() {
            QString err;
            if (gvRenderFilename(getContext(), getGraph(), qcstr(fmt), qcstr(n)))
                err = tr("cannot export as %1 in %2").arg(fmt, n);
            return err;
        });
    }
}

void lqXDotView::reloadLayout(QString newLayout)
{
    render_file(newLayout, "nop2");
}

void lqXDotView::setFoldedScene(lqXDotScene* s, QPointF p)
{
    Q_UNUSED(p)
    setRenderHint(QPainter::Antialiasing);
    setRenderHint(QPainter::TextAntialiasing);
    //clear();
    setScene(s);
    s->build();
    //translate(p.x(), p.y());
}

lqContextGraph* lqXDotView::factory_cg(GVC_t* context, Agraph_t *graph, lqXDotView* view) {
    return new lqContextGraph(context, graph, view);
}
lqContextGraph* lqXDotView::factory_cg(lqXDotView* view) {
    return new lqContextGraph(view);
}
lqXDotScene* lqXDotView::factory_scene(lqContextGraph* cg) {
    return new lqXDotScene(cg);
}
