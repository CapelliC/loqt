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

#ifndef LQGRAPHIXSCENE_H
#define LQGRAPHIXSCENE_H

#include <QGraphicsScene>
#include "primitives.h"

class lqGraphixScene : public QGraphicsScene
{
    Q_OBJECT
public:
    lqGraphixScene();
    lqGraphixScene(const lqGraphixScene &other);
    ~lqGraphixScene();

    Q_INVOKABLE lqGraphixEllipseItem *addEllipse(const QRectF &rect, const QPen &pen, const QBrush &brush);
    Q_INVOKABLE lqGraphixLineItem *addLine(const QLineF &line, const QPen &pen);
    Q_INVOKABLE lqGraphixPathItem *addPath(const QPainterPath &path, const QPen &pen, const QBrush &brush);
    Q_INVOKABLE lqGraphixPixmapItem *addPixmap(const QPixmap &pixmap);
    Q_INVOKABLE lqGraphixPolygonItem *addPolygon(const QPolygonF &polygon, const QPen &pen, const QBrush &brush);
    Q_INVOKABLE lqGraphixRectItem *addRect(const QRectF &rect, const QPen &pen, const QBrush &brush);
    Q_INVOKABLE lqGraphixSimpleTextItem *addSimpleText(const QString &text, const QFont &font);
};

#endif // LQGRAPHIXSCENE_H
