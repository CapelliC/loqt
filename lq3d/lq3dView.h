/*
    lq3d         : interfacing Qt3D

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


#ifndef LQ3DVIEW_H
#define LQ3DVIEW_H

#include "lq3d_global.h"
#include "lq3dScene.h"

#include <QWindow>
#include <Qt3DExtras/Qt3DWindow>

/** Graphviz library rendering using Qt Graphics View Framework
 */
class LQ3DSHARED_EXPORT lq3dView : public Qt3DExtras::Qt3DWindow  {

    Q_OBJECT

public:

    lq3dView(QScreen* screen = nullptr);
    virtual ~lq3dView();

    lq3dScene *scene = 0;

    void cylinderTest();
    void torusTest();

signals:

protected:
    virtual void keyPressEvent(QKeyEvent *e);

protected slots:

protected:
};

#endif // LQ3DVIEW_H
