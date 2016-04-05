/*
    lqShapes_test: SWI-Prolog and Qt Graphics Framework

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

#include "mainwindow.h"
#include "lqShapesView.h"
#include "pqMiniSyntax.h"
#include "QStackedWidget_KeybTabs.h"
#include "PREDICATE.h"
#include "file2string.h"

#include <QMenuBar>
#include <QFileDialog>
#include <QApplication>

static MainWindow *mainWindow() {
    for (auto w: qApp->topLevelWidgets())
        if (auto m = qobject_cast<MainWindow*>(w))
            return m;
    return 0;
}

QSplitter *MainWindow::splitter() const {
    return qobject_cast<QSplitter*>(centralWidget());
}
QStackedWidget *MainWindow::stacked() const {
    return qobject_cast<QStackedWidget*>(splitter()->widget(1));
}

lqShapesView* MainWindow::view() const {
    return qobject_cast<lqShapesView*>(splitter()->widget(0));
}
ConsoleEdit* MainWindow::console() const {
    return qobject_cast<ConsoleEdit*>(stacked()->widget(0));
}
QTextEdit* MainWindow::script() const {
    return qobject_cast<QTextEdit*>(stacked()->widget(1));
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
PREDICATE(script, 1) {
    if (auto m = mainWindow())
        return PL_A1 = toObj(m->script());
    return false;
}

MainWindow::MainWindow(int argc, char **argv, QWidget *parent)
    : QMainWindow(parent)
{
    LqShapes shapes;

    QWidget::setWindowFlags(Qt::Window | Qt::FramelessWindowHint);

    auto s = new QSplitter(Qt::Vertical);
    setCentralWidget(s);
    s->setHandleWidth(3);

    s->addWidget(new lqShapesView);
    auto S = new QStackedWidget_KeybTabs();
    s->addWidget(S);

    S->addWidget(new ConsoleEdit(argc, argv));
    S->addWidget(new QTextEdit);
    new pqMiniSyntax(script());

    s->setSizes({s->height() / 4 * 3, s->height() / 4 * 1});

    auto file = menuBar()->addMenu("&File");

    auto open = new QAction(QIcon::fromTheme("document-open"), tr("&Open script"), this);
    open->setShortcut(QKeySequence::Open);
    connect(open, &QAction::triggered, [&]() {
        QString cscript;
        if (qApp->arguments().size() == 2)
            cscript = qApp->arguments()[1];
        auto script = QFileDialog::getOpenFileName(this, tr("Select script"), cscript, "*.pl");
        if (!script.isEmpty() && script != cscript)
            setScript(script);
    });
    file->addAction(open);

    file->addSeparator();

    auto quit = new QAction(QIcon::fromTheme("application-exit"), tr("&Quit"), this);
    quit->setShortcut(QKeySequence::Quit);
    connect(quit, &QAction::triggered, [&]() { qApp->quit(); });
    file->addAction(quit);

    connect(qApp, &QApplication::aboutToQuit, []() { SwiPrologEngine::quit_request(); });
    if (qApp->arguments().size() == 2)
        connect(console(), &ConsoleEdit::engine_ready, [&]() {
            setScript(qApp->arguments()[1]);
        });
}

void MainWindow::setScript(QString file) {
    view()->loadScript(file);
    script()->setPlainText(file2string(file));
}

MainWindow::~MainWindow()
{

}
