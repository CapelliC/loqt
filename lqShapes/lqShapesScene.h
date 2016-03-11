/*
    lqShapes    : SWI-Prolog and Qt Graphics Framework

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

#ifndef LQSHAPESSCENE_H
#define LQSHAPESSCENE_H

#include <QGraphicsScene>
#include "lqShapes.h"

class LQSHAPESSHARED_EXPORT lqShapesScene : public QGraphicsScene
{
    Q_OBJECT
public:
    lqShapesScene();
    lqShapesScene(const lqShapesScene &other);
    ~lqShapesScene();

    Q_INVOKABLE lqShapesEllipseItem *addEllipse(const QRectF &rect, const QPen &pen, const QBrush &brush);
    Q_INVOKABLE lqShapesLineItem *addLine(const QLineF &line, const QPen &pen);
    Q_INVOKABLE lqShapesPathItem *addPath(const QPainterPath &path, const QPen &pen, const QBrush &brush);
    Q_INVOKABLE lqShapesPixmapItem *addPixmap(const QPixmap &pixmap);
    Q_INVOKABLE lqShapesPolygonItem *addPolygon(const QPolygonF &polygon, const QPen &pen, const QBrush &brush);
    Q_INVOKABLE lqShapesRectItem *addRect(const QRectF &rect, const QPen &pen, const QBrush &brush);
    Q_INVOKABLE lqShapesSimpleTextItem *addSimpleText(const QString &text, const QFont &font);
};

#endif // LQSHAPESSCENE_H
