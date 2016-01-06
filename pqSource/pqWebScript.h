/*
    pqSource      : SWI-Prolog Qt Rendering

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015

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

#ifndef PQWEBSCRIPT_H
#define PQWEBSCRIPT_H

#ifdef QT_WEBENGINE_LIB
    #include <QWebEngineView>
    typedef QWebEngineView WEB_VIEW_BASE;
#else
    #include <QWebView>
    typedef QWebView WEB_VIEW_BASE;
#endif

#include "pqSource.h"

class pqWebScript : public WEB_VIEW_BASE
{
    Q_OBJECT
public:

    explicit pqWebScript(QWidget *parent = 0);

    // code controlling this client
    QPointer<pqSource> server;

signals:

public slots:

    void startWebScript();

protected slots:
    /*
    void loadStarted();
    void loadProgress(int progress);
    void loadFinished(bool);
    void titleChanged(const QString& title);
    void statusBarMessage(const QString& text);
    void linkClicked(const QUrl&);
    void selectionChanged();
    void iconChanged();
    void urlChanged(const QUrl&);
    */
};

#endif // PQWEBSCRIPT_H
