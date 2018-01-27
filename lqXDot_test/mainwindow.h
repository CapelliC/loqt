/*
    lqXDot_test   : interfacing Qt and Graphviz library

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

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QTabWidget>
#include <QSplitter>
#include <QTextEdit>
#include <QMessageBox>
#include <QCloseEvent>
#include <QFileSystemWatcher>

#include "lqXDotView.h"
#include "lqGvSynCol.h"
#include "MruHelper.h"
#include "SvgView.h"
#include "RowColIndicators.h"
#include "ParenMatching.h"

/** display a single graph (dot file)
  * - lqXDotView main viewer
  * - the SVG rendering of the same
  * - DOT source file
  * - SVG source file
  */
class MainWindow : public QMainWindow, MruHelper
{
    Q_OBJECT
    
public:
    MainWindow(int argc, char *argv[], QWidget *parent = 0);
    ~MainWindow();

protected:

    virtual void closeEvent(QCloseEvent *e);

private:

    void saveViewDot(QString file, QString script, QString errmsg);

    void viewDot();
    QString fileDot;

    QString lastDir;
    QString lastMode;

    //enum t_kind { t_view, t_svgv, t_source, t_svgxml };
    enum t_kind { t_dot, t_svg };
    QPointer<QTabWidget> tabs;

    typedef ParenMatching::range range;
    range paren;

    //QSplitter* tab(t_kind t) const { return qobject_cast<V*>(tabs->widget(t)); }
    //template <class V> V* tab(t_kind t) const { return qobject_cast<V*>(tabs->widget(t)); }

    lqXDotView *view() const {
        QSplitter *s = qobject_cast<QSplitter*>(tabs->widget(t_dot));
        return qobject_cast<lqXDotView*>(s->widget(0));
    }
    QTextEdit *source() const {
        QSplitter *s = qobject_cast<QSplitter*>(tabs->widget(t_dot));
        return qobject_cast<QTextEdit*>(s->widget(1));
    }
    SvgView *svgv() const {
        QSplitter *s = qobject_cast<QSplitter*>(tabs->widget(t_svg));
        return qobject_cast<SvgView*>(s->widget(0));
    }
    QTextEdit *svgxml() const {
        QSplitter *s = qobject_cast<QSplitter*>(tabs->widget(t_svg));
        return qobject_cast<QTextEdit*>(s->widget(1));
    }

    enum { highlighting, editing } mode;
    RowColIndicators rci;

    void makeSvg(QString f = QString());

    typedef QMessageBox MB;
    MB::StandardButton errbox(QString msg, QString info,
        MB::StandardButtons bnts = MB::Ok,
        MB::Icon icon = MB::Critical, QString title = QString());

    QFileSystemWatcher monitorScript;

    QPointer<lqGvSynCol> gvsyn;

public slots:

    void newFile();
    void openFile();
    void saveFile();
    void saveFileAs();
    void renderFile();

    void openFileIndex(int);
    void changeLayout();

    void textChanged();
    void cursorPositionChanged();

    void scriptChanged(QString path);
};

#endif // MAINWINDOW_H
