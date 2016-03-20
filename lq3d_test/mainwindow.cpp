/*
    lq3D_test   : interfacing Qt and Graphviz library

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

#include "mainwindow.h"
#include "lqPreferences.h"

#include <QMenu>
#include <QMenuBar>
#include <QStatusBar>
#include <QApplication>
#include <QPlainTextEdit>
#include <QDebug>

/** GUI setup
 */
MainWindow::MainWindow(int, char *[], QWidget *parent)
    : QMainWindow(parent), MruHelper("lq3d_test")
{
    setCentralWidget(new QMdiArea(this));

    QMenu *m = menuBar()->addMenu("&File");
    m->addAction(tr("&New..."), this, &MainWindow::newFile, QKeySequence::New);
    m->addSeparator();
    m->addAction(tr("E&xit"), qApp, &QApplication::quit, QKeySequence::Quit);

    statusBar()->showMessage("Ready...");
}

void MainWindow::newFile()
{
    auto view = new lq3dView();

    static int t = 0;
    if (t++ % 2 == 0) {
        view->cylinderTest();
        statusBar()->showMessage("Created new cylinderTest");
    } else {
        view->torusTest();
        statusBar()->showMessage("Created new torusTest");
    }

    auto w = QWidget::createWindowContainer(view);
    mdiArea()->addSubWindow(w);
    w->show();

    mdiArea()->tileSubWindows();
}

/** save settings
 */
MainWindow::~MainWindow() {
    lqPreferences p;
    p.saveGeometry(this);
    p.save();
}

/** handle details on application quit
 */
void MainWindow::closeEvent(QCloseEvent *) {
    if (isWindowModified()) {

    }
}
