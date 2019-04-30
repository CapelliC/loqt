/*
    pqGraphviz    : interfacing SWI-Prolog and Graphviz library

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
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

#ifndef PQDOCVIEW_H
#define PQDOCVIEW_H

#include <QWebEngineView>
typedef QWebEngineView WEB_VIEW_BASE;

#include <QToolBar>
#include <QStatusBar>
#include <QPointer>

#include "pqGraphviz_global.h"

/** implements plDoc browser
 */
class PQGRAPHVIZSHARED_EXPORT pqDocView : public WEB_VIEW_BASE
{
    Q_OBJECT
public:

    //! display a message while waiting to start plDoc
    explicit pqDocView(QWidget *parent = 0);

    //! start plDoc server
    bool startPlDoc();

    //! display help on context topic
    void helpTopic(QString topic);

    //! display help on prolog file
    void helpFile(QString file);

    //! create commands in toolbar and connect messages to status bar
    void addFeedback(QToolBar *tbar, QStatusBar* sbar);

    //! qualify HTTP localhost port used
    enum { helpDocPort = 4001 };

private slots:
    void initUrl();

public slots:

    void linkHovered(QString,QString,QString);
    void loadFinished(bool);
    void loadProgress(int);

private:

    QPointer<QToolBar> toolBar;
    QPointer<QStatusBar> statusBar;

    QStringList requests;
};

#endif // PQDOCVIEW_H
