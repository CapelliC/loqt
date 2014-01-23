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

#ifndef QPPMAINWINDOW_H
#define QPPMAINWINDOW_H

#include "MruHelper.h"
#include "ConsoleEdit.h"
#include "qppViewFile.h"

#include <QList>
#include <QFile>
#include <QTimer>
#include <QPointer>
#include <QSplitter>
#include <QTabWidget>
#include <QMainWindow>
#include <QApplication>
#include <QStateMachine>

/** qPrologPad application main window:
 *  keep a SWI-Prolog console at left, and tabbed source files at right.
 *  Each source file is hosted by CodeMirror, with semantic syntax highlighting handled by PrologPad.
 */
class qppMainWindow : public QMainWindow, MruHelper
{
    Q_OBJECT

public:

    qppMainWindow(int argc, char *argv[], QWidget *parent = 0);
    ~qppMainWindow();

    void editFile(QString full);
    ConsoleEdit *console() const { return edit; }

    void err(QString title, QString message);
    void msgt(QString message) { emit(msgtSig(message)); }

protected:
    virtual void closeEvent(QCloseEvent *event);
    virtual void keyPressEvent(QKeyEvent* e);

private:
    QPointer<QSplitter> split;
    QPointer<ConsoleEdit> edit;
    QPointer<QTabWidget> tab;

    int findTab(QString path) const;

public slots:
    void msg(QString message);

protected slots:

    void openFile();
    void closeFile();
    void newFile();
    void saveFile();
    void openSourceFile(QString file);
    void openFileIndex(int);
    void quitRequest();

    void setTitle(QString file, QString title);

signals:

    void openSourceFileSig(QString file);
    void msgtSig(QString s);
};

//! let components access top level container in easy way
inline qppMainWindow* topw() {
    foreach (QWidget *widget, QApplication::topLevelWidgets())
        if (qobject_cast<qppMainWindow*>(widget))
            return qobject_cast<qppMainWindow*>(widget);
        return 0;
}

#endif // QPPMAINWINDOW_H
