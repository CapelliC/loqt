/*
    spqr          : SWI-Prolog Qt Rendering

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
    Copyright (C) : 2013, Carlo Capelli

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

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QTextEdit>
#include <QMessageBox>
#include <QCloseEvent>
#include <QFileSystemWatcher>
#include <QWebView>
#include <QPointer>

#include "MruHelper.h"
#include "ConsoleEdit.h"
#include "SourceEdit.h"
#include "HelpDocView.h"
#include "QStackedWidget_KeybTabs.h"

/** display a single graph (dot file)
  * - Source : Prolog script, keep same entry point as filename
  * - Console: Prolog debug/error report
  * - Help   : live SWI-Prolog plDoc documentation server
  */
class MainWindow : public QMainWindow, MruHelper
{
    Q_OBJECT

public:

    MainWindow(int argc, char *argv[], QWidget *parent = 0);
    ~MainWindow();

    enum t_kind { t_source, t_console, t_helpdoc };
    template <class V> V* tab(t_kind t) const { return qobject_cast<V*>(tabs->widget(t)); }
    void activate(t_kind k) { tabs->setCurrentIndex(k); }

    SourceEdit*     source() const { return tab<SourceEdit>(t_source); }
    ConsoleEdit*    console() const { return tab<ConsoleEdit>(t_console); }
    HelpDocView*    helpDoc() const { return tab<HelpDocView>(t_helpdoc); }

    //! access  http://localhost document server
    enum { DOC_PORT = 4000 };

protected:

    virtual void closeEvent(QCloseEvent *e);

private:

    void saveViewDot(QString file, QString script, QString errmsg);

    void viewDot();
    QString fileSource;
    QString lastDir;
    //QString lastMode;

    //! keep just 1 console
    QPointer<ConsoleEdit> con;

    //! slick interface - move tabs on QMenu
    QPointer<QStackedWidget_KeybTabs> tabs;

    typedef ParenMatching::range range;
    range paren;

private:

    enum { highlighting, editing } mode;

    void saveFile(QString file);

    typedef QMessageBox MB;
    MB::StandardButton errbox(QString msg, QString info, MB::StandardButtons bnts = MB::Ok, MB::Icon icon = MB::Critical, QString title = QString());

    QFileSystemWatcher monitorScript;
    void make_tabs();

    QPointer<QLineEdit> locationEdit;
    int progress;

public slots:

    void newFile();
    void openFile();
    void saveFile();
    void saveFileAs();

    void openFileIndex(int);
    void changeLayout();

    void textChanged();

    void scriptChanged(QString path);
    void engineReady();

    void log(QString msg);
    void msg(QString msg);
    void err(QString msg);

protected slots:

    void adjustLocation();
    void changeLocation();
    void adjustTitle();
    void setProgress(int p);
    void finishLoading(bool);

    void viewGraph();
    void viewSource();
    void viewConsole();
    void viewHelp();

    void queryComplete(QString query, int tot_occurrences);
    void queryException(QString functor, QString exmsg);
};

#endif // MAINWINDOW_H
