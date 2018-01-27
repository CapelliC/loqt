/*
    lqGraphix    : SWI-Prolog and Qt Graphics Framework
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

#include "mainwindow.h"
#include "lqShapesView.h"
#include "lqShapes.h"
#include "PREDICATE.h"

#include <QMenuBar>
#include <QApplication>
#include <QFileDialog>

static MainWindow *mainWindow() {
    for (auto w: qApp->topLevelWidgets())
        if (auto m = qobject_cast<MainWindow*>(w))
            return m;
    return 0;
}

structure2(pqObj)
inline T toObj(QObject* obj) { return pqObj(A(obj->metaObject()->className()), obj); }

PREDICATE(view, 1) {
    if (auto m = mainWindow())
        return PL_A1 = toObj(m->view());
    return false;
}
PREDICATE(scene, 1) {
    if (auto m = mainWindow())
        return PL_A1 = toObj(m->view()->scene());
    return false;
}

MainWindow::MainWindow(int argc, char **argv, QWidget *parent)
    : QMainWindow(parent)
{
    LqShapes shapes;

    auto s = new QSplitter(Qt::Vertical);
    setCentralWidget(s);
    s->setHandleWidth(3);

    s->addWidget(new lqShapesView);
    s->addWidget(new ConsoleEdit(argc, argv));

    s->setSizes({s->height() / 4 * 3, s->height() / 4 * 1});

    auto file = menuBar()->addMenu("&File");

    auto open = new QAction("&Open script", this);
    open->setShortcut(QKeySequence::Open);
    connect(open, &QAction::triggered, [&]() {
        QString cscript;
        if (qApp->arguments().size() == 2)
            cscript = qApp->arguments()[1];
        auto script = QFileDialog::getOpenFileName(this, "Select script", cscript, "*.pl");
        if (!script.isEmpty() && script != cscript)
            view()->loadScript(script);
    });
    file->addAction(open);

    file->addSeparator();

    auto quit = new QAction("&Quit", this);
    quit->setShortcut(QKeySequence::Quit);
    connect(quit, &QAction::triggered, [&]() { qApp->quit(); });
    file->addAction(quit);

    connect(qApp, &QApplication::aboutToQuit, []() { SwiPrologEngine::quit_request(); });
    if (qApp->arguments().size() == 2)
        connect(console(), &ConsoleEdit::engine_ready, [&]() {
            view()->loadScript(qApp->arguments()[1]);
        });
}

MainWindow::~MainWindow()
{

}
