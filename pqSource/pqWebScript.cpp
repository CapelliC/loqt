/*
    pqSource      : SWI-Prolog Qt Rendering

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
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
#ifndef QT_WEBENGINE_LIB

    // this will issue a developer environment creation
    page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);

    // simpler browser events...
    connect(this, &QWebView::titleChanged, [=](const QString &t) { setWindowTitle(t); });
#endif
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
