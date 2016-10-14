/*
    pqConsole    : interfacing SWI-Prolog and Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016

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

#ifndef PQPROOFVIEW_H
#define PQPROOFVIEW_H

#include <QGraphicsView>
#include <QMouseEvent>
#include "pqProofScene.h"

/**
 * @brief The pqProofView class
 *  display a Prolog proof tree
 */
class PQCONSOLESHARED_EXPORT pqProofView : public QGraphicsView
{
    Q_OBJECT

public:
    pqProofView();
    ~pqProofView();

    pqProofScene* scene() const { return qobject_cast<pqProofScene*>(QGraphicsView::scene()); }

protected:
    void wheelEvent(QWheelEvent* event);
    void scale_view(qreal scaleFactor);
};

#endif // PQPROOFVIEW_H