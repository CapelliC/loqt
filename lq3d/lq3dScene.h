/*
    lq3D         : interfacing Qt3D

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016,2017,2018

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

#ifndef LQ3DSCENE_H
#define LQ3DSCENE_H

#include <Qt3DInput/QInputAspect>
#include <Qt3DExtras/QForwardRenderer>

#include "lq3dContext.h"
#include "lq3dObj.h"
class lq3dView;

/** translate a Graphviz graph to a QGraphicScene display graph
  * each graph object get its concrete representation as QGraphicsItemGroup
  */
class LQ3DSHARED_EXPORT lq3dScene : public QObject
{
    Q_OBJECT

public:

    lq3dScene(lq3dContext *cg = 0);

    lq3dContext *cg = 0;
    Qt3DCore::QEntity *rootEntity = 0;

    Qt3DRender::QCamera *cameraEntity = 0;
    Qt3DRender::QFrameGraphNode *frameGraph = 0;
    Qt3DExtras::QForwardRenderer *forwardRenderer = 0;
};

#endif // LQ3DSCENE_H
