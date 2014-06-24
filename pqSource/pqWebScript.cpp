/*
    pqSource      : SWI-Prolog Qt Rendering

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
    Copyright (C) : 2013,2014 Carlo Capelli

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

pqWebScript::pqWebScript(QWidget *parent) :
    QWebView(parent)
{
    page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
    connect(this, &QWebView::titleChanged, [=](const QString &t) { setWindowTitle(t); });
    QTimer::singleShot(1, this, SLOT(startWebScript()));
}

void pqWebScript::startWebScript()
{
    auto l = server->startWebScript();
    for (auto m: l)
        qDebug() << m;
    if (!l.isEmpty())
        load(l[0]);
}
