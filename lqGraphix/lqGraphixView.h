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

#ifndef LQGRAPHIXVIEW_H
#define LQGRAPHIXVIEW_H

#include <QGraphicsView>
#include "lqGraphixScene.h"

class lqGraphixView : public QGraphicsView
{
    Q_OBJECT
public:
    lqGraphixView(QWidget *parent = Q_NULLPTR);

    //! fulfill qRegisterMetaType requirements
    lqGraphixView(const lqGraphixView &other);
    ~lqGraphixView();

    lqGraphixScene* scene() const { return qobject_cast<lqGraphixScene*>(QGraphicsView::scene()); }

    bool loadScript(QString script);
};

#endif // LQGRAPHIXVIEW_H
