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

#include "lq3dScene.h"
#include "lqLogger.h"
#include "lqAniMachine.h"
#include "lq3dView.h"

#include "lq3d_configure.h"

#include <QTime>
#include <QDebug>
#include <QFinalState>
#include <QSignalMapper>
#include <QStateMachine>

lq3dScene::lq3dScene(lq3dContext *cg) : cg(cg)
{
    rootEntity = new Qt3DCore::QEntity;
    cameraEntity = new Qt3DRender::QCamera(rootEntity);
    cameraEntity->setObjectName(QStringLiteral("cameraEntity"));

    cameraEntity->lens()->setPerspectiveProjection(45.0f, 16.0f/9.0f, 0.1f, 1000.0f);
    cameraEntity->setPosition(QVector3D(0, 0, -20.0f));
    cameraEntity->setUpVector(QVector3D(0, 1, 0));
    cameraEntity->setViewCenter(QVector3D(0, 0, 0));
    //input->setCamera(cameraEntity);

    // FrameGraph
    frameGraph = new Qt3DRender::QFrameGraphNode;
    forwardRenderer = new Qt3DExtras::QForwardRenderer;

    // TechiqueFilter and renderPassFilter are not implement yet
    forwardRenderer->setCamera(cameraEntity);
    forwardRenderer->setClearColor(Qt::black);

    //frameGraph->setActiveFrameGraph(forwardRenderer);

    // Setting the FrameGraph
    //rootEntity->addComponent(frameGraph);
}
