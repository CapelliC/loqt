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

#ifndef LQSHAPES_H
#define LQSHAPES_H

#include "lqShapes_global.h"

#include <QGraphicsItem>
#include <QMap>

class LQSHAPESSHARED_EXPORT LqShapes
{
public:
    LqShapes();
    QMap<QString, int> metatypes;
};

#define meta(Cla) class lqShapes##Cla : public QGraphics##Cla { \
    public: \
        lqShapes##Cla() {} \
        lqShapes##Cla(const lqShapes##Cla &) : QGraphics##Cla() {} \
        ~lqShapes##Cla() {} \
};

meta(RectItem)
/*
class lqShapesRectItem : public QGraphicsRectItem, public QGraphicsObject {
    Q_OBJECT
    public:
        lqShapesRectItem() {}
        lqShapesRectItem(const lqShapesRectItem &) : QGraphicsRectItem() {}
        ~lqShapesRectItem() {}
};
*/
meta(EllipseItem)
meta(PathItem)
meta(PolygonItem)
meta(SimpleTextItem)
meta(LineItem)
meta(PixmapItem)

#endif // LQSHAPES_H
