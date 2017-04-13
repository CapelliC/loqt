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

#include "lq3dView.h"

#include <QKeyEvent>
#include <QGuiApplication>
#include <QOpenGLContext>
#include <Qt3DExtras/QFirstPersonCameraController>
#include <Qt3DExtras/QCylinderMesh>
#include <Qt3DExtras/QTorusMesh>

/** actual constructor, make an empty view
 */
lq3dView::lq3dView(QScreen *screen)
    : Qt3DExtras::Qt3DWindow(screen)
{
    //defaultFramegraph()->setClearColor(QColor(QRgb(0x404040)));
    resize(800, 600);

    /*
    setSurfaceType(QSurface::OpenGLSurface);

    QSurfaceFormat format;
    if (QOpenGLContext::openGLModuleType() == QOpenGLContext::LibGL) {
        format.setVersion(4, 3);
        format.setProfile(QSurfaceFormat::CoreProfile);
    }
    format.setDepthBufferSize(24);
    format.setSamples(4);
    format.setStencilBufferSize(8);
    setFormat(format);
    create();
    */

    scene = new lq3dScene(new lq3dContext);

    Qt3DExtras::QFirstPersonCameraController *camController = new Qt3DExtras::QFirstPersonCameraController(scene->rootEntity);
    camController->setCamera(scene->cameraEntity);
}

lq3dView::~lq3dView()
{
    delete scene;
}

void lq3dView::keyPressEvent(QKeyEvent* e)
{
    switch (e->key()) {
    case Qt::Key_Escape:
        QGuiApplication::quit();
        break;
    default:
        QWindow::keyPressEvent(e);
    }
}

void lq3dView::cylinderTest() {
    auto rootEntity = scene->rootEntity;

    auto cylinder = new Qt3DExtras::QCylinderMesh;
    cylinder->setRadius(1);
    cylinder->setLength(3);
    cylinder->setRings(100);
    cylinder->setSlices(20);

    // CylinderMesh Transform
    auto cylinderTransform = new Qt3DCore::QTransform;
    cylinderTransform->setScale(1.5f);
    cylinderTransform->setRotation(QQuaternion::fromAxisAndAngle(QVector3D(1, 0, 0), 45.0f));

    // Cylinder
    auto cylinderEntity = new Qt3DCore::QEntity(rootEntity);
    cylinderEntity->addComponent(cylinder);
    cylinderEntity->addComponent(cylinderTransform);
}

void lq3dView::torusTest() {
    auto rootEntity = scene->rootEntity;

    auto torus = new Qt3DExtras::QTorusMesh;
    torus->setMinorRadius(1);
    torus->setRadius(3);
    torus->setRings(100);
    torus->setSlices(20);

    // CylinderMesh Transform
    auto torusTransform = new Qt3DCore::QTransform;
    torusTransform->setScale(1.5f);
    torusTransform->setRotation(QQuaternion::fromAxisAndAngle(QVector3D(1, 0, 0), 45.0f));

    // Cylinder
    auto torusEntity = new Qt3DCore::QEntity(rootEntity);
    torusEntity->addComponent(torus);
    torusEntity->addComponent(torusTransform);
}
