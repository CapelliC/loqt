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

#include <QMap>
#include <QGraphicsItem>
#include <QGraphicsProxyWidget>

#include <QPointer>
#include <QPushButton>

class LQSHAPESSHARED_EXPORT LqShapes
{
public:
    LqShapes();
    QMap<QString, int> metatypes;
};

#define meta(Cla) class LQSHAPESSHARED_EXPORT  lqShapes##Cla : public QGraphics##Cla { \
    public: \
        lqShapes##Cla() {} \
        lqShapes##Cla(const lqShapes##Cla &) : QGraphics##Cla() {} \
        ~lqShapes##Cla() {} \
    }; \
    Q_DECLARE_METATYPE(lqShapes##Cla*)

//meta(RectItem)
class lqShapesRectItem : public QGraphicsObject {
    Q_OBJECT
    Q_PROPERTY(QRectF rect READ rect WRITE setRect)
public:
    QGraphicsRectItem *item;

    lqShapesRectItem();
    ~lqShapesRectItem();

    QRectF rect() const;
    void setRect(QRectF r);


    virtual QRectF boundingRect() const;
    virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget = Q_NULLPTR);
};

meta(EllipseItem)
meta(PathItem)
meta(PolygonItem)
meta(SimpleTextItem)
meta(LineItem)
meta(PixmapItem)

meta(ItemGroup)
meta(TextItem)
meta(ProxyWidget)

class LQSHAPESSHARED_EXPORT lqPushButton : public QObject {
    Q_OBJECT
public:

    lqPushButton();
    lqPushButton(const lqPushButton &other);
    ~lqPushButton();

    QPointer<QPushButton> pushButton;
};
Q_DECLARE_METATYPE(lqPushButton)

#endif // LQSHAPES_H
