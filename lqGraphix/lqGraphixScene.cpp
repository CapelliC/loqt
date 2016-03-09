/*
    lqGraphix    : SWI-Prolog and Qt Graphics Framework

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

#include "lqGraphixScene.h"
#include <QDebug>

lqGraphixScene::lqGraphixScene() {
    qDebug() << "lqGraphixScene";
}
lqGraphixScene::lqGraphixScene(const lqGraphixScene &) : QGraphicsScene() {
    qDebug() << "lqGraphixScene(const lqGraphixScene &)";
}
lqGraphixScene::~lqGraphixScene() {
    qDebug() << "~lqGraphixScene";
}

lqGraphixRectItem *lqGraphixScene::addRect(const QRectF &rect, const QPen &pen, const QBrush &brush)
{
    auto r = new lqGraphixRectItem;
    r->setRect(rect);
    r->setPen(pen);
    r->setBrush(brush);
    addItem(r);
    return r;
}

lqGraphixEllipseItem *lqGraphixScene::addEllipse(const QRectF &rect, const QPen &pen, const QBrush &brush)
{
    qDebug() << "addEllipse" << rect << pen << brush;
    auto e = new lqGraphixEllipseItem;
    e->setRect(rect);
    e->setPen(pen);
    e->setBrush(brush);
    addItem(e);
    return e;
}

lqGraphixLineItem *lqGraphixScene::addLine(const QLineF &line, const QPen &pen)
{
    qDebug() << "addLine" << line << pen;
    auto l = new lqGraphixLineItem;
    l->setLine(line);
    l->setPen(pen);
    addItem(l);
    return l;
}

lqGraphixPathItem *lqGraphixScene::addPath(const QPainterPath &path, const QPen &pen, const QBrush &brush)
{
    qDebug() << "addPath" << path << pen << brush;
    auto p = new lqGraphixPathItem;
    p->setPath(path);
    p->setPen(pen);
    p->setBrush(brush);
    addItem(p);
    return p;
}

lqGraphixPixmapItem *lqGraphixScene::addPixmap(const QPixmap &pixmap)
{
    qDebug() << "addPixmap" << pixmap;
    auto p = new lqGraphixPixmapItem;
    p->setPixmap(pixmap);
    addItem(p);
    return p;
}

lqGraphixPolygonItem *lqGraphixScene::addPolygon(const QPolygonF &polygon, const QPen &pen, const QBrush &brush)
{
    qDebug() << "addPolygon" << polygon << pen << brush;
    auto p = new lqGraphixPolygonItem;
    p->setPolygon(polygon);
    p->setPen(pen);
    p->setBrush(brush);
    addItem(p);
    return p;
}

lqGraphixSimpleTextItem *lqGraphixScene::addSimpleText(const QString &text, const QFont &font)
{
    qDebug() << "addText" << text << font;
    auto t = new lqGraphixSimpleTextItem;
    t->setText(text);
    t->setFont(font);
    addItem(t);
    return t;
}
