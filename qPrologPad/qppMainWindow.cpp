/*
    qPrologPad   : SWI-Prolog PrologPad in Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2014 Carlo Capelli

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

#include "qppMainWindow.h"
#include "ConsoleEdit.h"
#include "lqPreferences.h"

#include <QMenuBar>
#include <QStatusBar>
#include <QApplication>
#include <QCloseEvent>
#include <QFileDialog>
#include <QTextStream>
#include <QMessageBox>
#include <QTime>

qppMainWindow::qppMainWindow(int argc, char *argv[], QWidget *parent)
    : QMainWindow(parent) {

    setCentralWidget(split = new QSplitter(this));
    split->addWidget(edit = new ConsoleEdit(argc, argv));
    split->addWidget(tab = new QTabWidget);

    QMenu *File = menuBar()->addMenu(tr("&File"));

    File->addAction(tr("&Open..."), this, SLOT(openFile()), QKeySequence::Open);
    File->addAction(tr("&New..."), this, SLOT(newFile()), QKeySequence::New);

    File->addAction(tr("&Save"), this, SLOT(saveFile()), QKeySequence::Save);
    File->addAction(tr("&Close"), this, SLOT(closeFile()), QKeySequence::Close);
    File->addSeparator();

    lqPreferences s;
    loadMru(s, this);

    //s.read(this, "window");
    split->restoreState(s.value("split_console").toByteArray());
    //mru->focus = s.value("current_source").toInt();

    File->addSeparator();
    File->addAction(tr("&Quit"), this, SLOT(quitRequest()), QKeySequence::Quit);

    QMenu* Debug = menuBar()->addMenu(tr("&Debug"));

    using namespace Qt;
    Debug->addAction(tr("&Compile"), this, SLOT(Compile()), Key_F2);
    Debug->addAction(tr("&Trace"), this, SLOT(Trace()), CTRL+Key_F5);
    Debug->addAction(tr("&Run"), this, SLOT(Run()), Key_F5);
    Debug->addAction(tr("&Break"), this, SLOT(Break()), CTRL+Key_SysReq);
    Debug->addAction(tr("&Toggle Breakpoint"), this, SLOT(ToggleBreakpoint()), Key_F9);
    Debug->addAction(tr("&Step Over"), this, SLOT(StepOver()), Key_F10);
    Debug->addAction(tr("Step &Into"), this, SLOT(StepInto()), Key_F11);
    Debug->addAction(tr("Step &Out"), this, SLOT(StepOut()), SHIFT+Key_F11);

    connect(this, SIGNAL(openSourceFileSig(QString)), this, SLOT(openSourceFile(QString)));
    connect(this, SIGNAL(msgtSig(QString)), this, SLOT(msg(QString)));
}

qppMainWindow::~qppMainWindow() {
    lqPreferences p;
    p.saveGeometry(this);
    //p.setValue("fileSource", source()->path());
    //p.setValue("lastDir", lastDir);
    storeMru(p);
    p.setValue("split_console", split->saveState());
    p.save();

    delete edit;
}

void qppMainWindow::quitRequest() {
    QCloseEvent e;
    closeEvent(&e);
    if (e.isAccepted())
        qApp->quit();
}

/** this is default as required by MruHelper
 */
void qppMainWindow::openFileIndex(int i) {
    openSourceFile(files[i]);
}

int qppMainWindow::findTab(QString path) const {
    for (int p = 0; p < tab->count(); ++p)
        if (tab->tabToolTip(p) == path)
            return p;
    return -1;
}

void qppMainWindow::closeEvent(QCloseEvent *event) {
    for (int t = 0; t < tab->count(); ++t)
        if (auto *e = qobject_cast<qppViewFile*>(tab->widget(t)))
            ; /*
            if (!e->maybeSave()) {
                event->ignore();
                return;
            }
            */

    event->accept();
}

void qppMainWindow::openFile() {
    foreach (QString f, QFileDialog::getOpenFileNames(this, tr("Open Source")))
        editFile(f);
}
void qppMainWindow::closeFile() {
    if (auto e = qobject_cast<qppViewFile *>(tab->currentWidget())) {
        /*
        if (e->isModified())
            e->save();
            */
        tab->removeTab(tab->currentIndex());
    }
}

void qppMainWindow::newFile() {
    QString n = QFileDialog::getSaveFileName(this, tr("New File"));
    if (!n.isNull()) {
        QFile f(n);
        if (f.open(QFile::WriteOnly)) {
            {   QTextStream s(&f);
                //s << ReflexQtPl().newFileHeader(n) << endl;
            }
            f.close();
            openSourceFile(n);
        }
    }
}

void qppMainWindow::saveFile() {
}

void qppMainWindow::editFile(QString file) {
    emit(openSourceFileSig(file));
}

void qppMainWindow::openSourceFile(QString file) {

    /*
    QAction *before = 0, *curr = 0;
    foreach (auto a, mru->files->actions())
        if (a->toolTip() == file) {
            for (int i = 0, n = tab->count(); i < n; ++i)
                if (tab->tabToolTip(i) == file) {
                    tab->setCurrentIndex(i);
                    return;
                }
            curr = a;
            break;
        }
        else if (a->toolTip() > file) {
            before = a;
            break;
        }

    QString title = path2title(file);
    if (!curr) {
        QAction *n = new QAction(this);
        n->setText(title);
        n->setToolTip(file);
        connect(n, SIGNAL(triggered()), this, SLOT(openMru()));
        if (before)
            mru->files->insertAction(before, n);
        else
            mru->files->addAction(n);
    }

    auto e = new qppViewFile(); //(file);
    int p = tab->addTab(e, title);
    tab->setTabToolTip(p, file);
    tab->setCurrentIndex(p);
    connect(e, SIGNAL(setTitle(QString, QString)), this, SLOT(setTitle(QString, QString)));
    */
}

void qppMainWindow::setTitle(QString file, QString title) {
    int t = findTab(file);
    if (t >= 0)
        tab->setTabText(t, title);
    /*
    int m = findMru(file);
    if (m >= 0)
        mru->files->actions().at(m)->setText(title);
        */
}

void qppMainWindow::msg(QString message) {
    //log(message);
    statusBar()->showMessage(message);
}

void qppMainWindow::err(QString title, QString message) {
    //log(title + ":" + message);
    QMessageBox::critical(this, title, message);
}

void qppMainWindow::keyPressEvent(QKeyEvent* e) {
    QKeySequence
        next(Qt::CTRL + Qt::Key_PageDown),
        prev(Qt::CTRL + Qt::Key_PageUp),
        curr(e->modifiers() + e->key());
    if (curr.matches(next) || curr.matches(prev)) {
        if (tab->hasFocus())
            edit->setFocus();
        else
            tab->setFocus();
        e->accept();
    }
}
