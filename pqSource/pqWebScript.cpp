/*
    pqSource      : SWI-Prolog Qt Rendering

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
    Copyright @ 2023

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

#include "pqWebScript.h"
#include <QDebug>

/**
 * @brief pqWebScript::pqWebScript
 *  Setup a WebScript window.
 * @param parent
 *  This is the QWidget parent, if any
 *  In a MDI environment, a view is usually attached to a QMdiChildWindow...
 */
pqWebScript::pqWebScript(QWidget *parent) :
    WEB_VIEW_BASE(parent)
{
    // server creation must be delayed after display
    QTimer::singleShot(1, this, SLOT(startWebScript()));
}

/**
 * @brief pqWebScript::startWebScript
 *  Request the source window to start a server.
 *  Then load first HTTP entry point
 */
void pqWebScript::startWebScript()
{
    auto l = server->startWebScript();
    for (auto m: l)
        qDebug() << m;
    if (!l.isEmpty())
        load(l[0]);
}
