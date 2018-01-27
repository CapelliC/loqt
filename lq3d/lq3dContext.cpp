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

#include "lq3dContext.h"
#include "lq3dScene.h"
#include "lq3d_configure.h"

#include <Qt3DRender>

/** allocate empty
 */
lq3dContext::lq3dContext(QObject *parent) :
    QObject(parent)
{
    engine.registerAspect(new Qt3DRender::QRenderAspect);
}

Qt3DInput::QInputAspect* lq3dContext::registerView(QWindow* view)
{
    Qt3DInput::QInputAspect *input = new Qt3DInput::QInputAspect;
    engine.registerAspect(input);
    QVariantMap data;
    data.insert(QStringLiteral("surface"), QVariant::fromValue(static_cast<QSurface *>(view)));
    data.insert(QStringLiteral("eventSource"), QVariant::fromValue(view));
#if QT_VERSION < QT_VERSION_CHECK(5, 7, 0)
    engine.setData(data);
#endif
    return input;
}
