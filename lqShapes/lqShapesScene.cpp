/*
    lqShapes     : SWI-Prolog and Qt Graphics Framework

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2016

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

#include "lqShapesScene.h"
#include <QDebug>

lqShapesScene::lqShapesScene() {
    qDebug() << "lqShapesScene";
}
lqShapesScene::lqShapesScene(const lqShapesScene &) : QGraphicsScene() {
    qDebug() << "lqShapesScene(const lqShapesScene &)";
}
lqShapesScene::~lqShapesScene() {
    qDebug() << "~lqShapesScene";
}

lqShapesRectItem *lqShapesScene::addRect(const QRectF &rect, const QPen &pen, const QBrush &brush)
{
    qDebug() << "addRect" << rect << pen << brush;
    auto r = new lqShapesRectItem;
    r->item->setRect(rect);
    r->item->setPen(pen);
    r->item->setBrush(brush);
    addItem(r);
    return r;
}

lqShapesEllipseItem *lqShapesScene::addEllipse(const QRectF &rect, const QPen &pen, const QBrush &brush)
{
    qDebug() << "addEllipse" << rect << pen << brush;
    auto e = new lqShapesEllipseItem;
    e->setRect(rect);
    e->setPen(pen);
    e->setBrush(brush);
    addItem(e);
    return e;
}

lqShapesLineItem *lqShapesScene::addLine(const QLineF &line, const QPen &pen)
{
    qDebug() << "addLine" << line << pen;
    auto l = new lqShapesLineItem;
    l->setLine(line);
    l->setPen(pen);
    addItem(l);
    return l;
}

lqShapesPathItem *lqShapesScene::addPath(const QPainterPath &path, const QPen &pen, const QBrush &brush)
{
    qDebug() << "addPath" << path << pen << brush;
    auto p = new lqShapesPathItem;
    p->setPath(path);
    p->setPen(pen);
    p->setBrush(brush);
    addItem(p);
    return p;
}

lqShapesPixmapItem *lqShapesScene::addPixmap(const QPixmap &pixmap)
{
    qDebug() << "addPixmap" << pixmap;
    auto p = new lqShapesPixmapItem;
    p->setPixmap(pixmap);
    addItem(p);
    return p;
}

lqShapesPolygonItem *lqShapesScene::addPolygon(const QPolygonF &polygon, const QPen &pen, const QBrush &brush)
{
    qDebug() << "addPolygon" << polygon << pen << brush;
    auto p = new lqShapesPolygonItem;
    p->setPolygon(polygon);
    p->setPen(pen);
    p->setBrush(brush);
    addItem(p);
    return p;
}

lqShapesSimpleTextItem *lqShapesScene::addSimpleText(const QString &text, const QFont &font)
{
    qDebug() << "addText" << text << font;
    auto t = new lqShapesSimpleTextItem;
    t->setText(text);
    t->setFont(font);
    addItem(t);
    return t;
}

lqShapesItemGroup *lqShapesScene::addGroup() {
    qDebug() << "addGroup";
    auto g = new lqShapesItemGroup;
    addItem(g);
    return g;
}
lqShapesTextItem *lqShapesScene::addText(const QString &text, const QFont &font) {
    auto t = new lqShapesTextItem;
    t->setPlainText(text);
    t->setFont(font);
    addItem(t);
    return t;
}

lqShapesProxyWidget *lqShapesScene::addProxyWidget(QWidget *widget) {
    auto w = new lqShapesProxyWidget;
    w->setWidget(widget);
    addItem(w);
    return w;
}
