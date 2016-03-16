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

#include "lqShapesView.h"
#include <QDebug>

LqShapes::LqShapes()
{
    #define reg(T) metatypes[#T "*"] = qRegisterMetaType<T*>(#T "*");
    reg(lqShapesView)
    reg(lqShapesScene)

    reg(lqShapesRectItem)
    reg(lqShapesEllipseItem)
    reg(lqShapesPathItem)
    reg(lqShapesPolygonItem)
    reg(lqShapesSimpleTextItem)
    reg(lqShapesLineItem)
    reg(lqShapesPixmapItem)

    reg(lqShapesItemGroup)
    reg(lqShapesTextItem)
    reg(lqShapesProxyWidget)

    reg(lqShapesProxyWidget)

    metatypes["lqPushButton"] = qRegisterMetaType<lqPushButton>("lqPushButton");

    #undef reg
}

lqShapesRectItem::lqShapesRectItem() {
    item = new QGraphicsRectItem;
}
lqShapesRectItem::~lqShapesRectItem() {
    delete item;
}

QRectF lqShapesRectItem::rect() const {
    return item->rect();
}
void lqShapesRectItem::setRect(QRectF r) {
    item->setRect(r);
}
QRectF lqShapesRectItem::boundingRect() const {
    return item->boundingRect();
}
void lqShapesRectItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
    item->paint(painter, option, widget);
}

lqPushButton::lqPushButton() {
    qDebug() << "lqPushButton::lqPushButton()";
    pushButton = new QPushButton;
}
lqPushButton::lqPushButton(const lqPushButton &other) : QObject() {
    qDebug() << "lqPushButton::lqPushButton(const lqPushButton &other)" << other.pushButton;
}
lqPushButton::~lqPushButton() {
    qDebug() << "lqPushButton::~lqPushButton()";
    delete pushButton;
}
