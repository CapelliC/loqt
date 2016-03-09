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

#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include <QGraphicsItem>
/*
#include <QGraphicsRectItem>
#include <QGraphicsEllipseItem>
#include <QGraphicsPathItem>
#include <QGraphicsPolygonItem>
#include <QGraphicsTextItem>
#include <QGraphicsLineItem>
#include <QGraphicsPixmapItem>
*/

#define meta(Cla) class lqGraphix##Cla : public QGraphics##Cla { \
    public: \
        lqGraphix##Cla() {} \
        lqGraphix##Cla(const lqGraphix##Cla &) : QGraphics##Cla() {} \
        ~lqGraphix##Cla() {} \
};

meta(RectItem)
/*
class lqGraphixRectItem : public QGraphicsRectItem, public QGraphicsObject {
    Q_OBJECT
    public:
        lqGraphixRectItem() {}
        lqGraphixRectItem(const lqGraphixRectItem &) : QGraphicsRectItem() {}
        ~lqGraphixRectItem() {}
};
*/
meta(EllipseItem)
meta(PathItem)
meta(PolygonItem)
meta(SimpleTextItem)
meta(LineItem)
meta(PixmapItem)

#endif // PRIMITIVES_H
