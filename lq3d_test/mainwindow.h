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

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QTabWidget>
#include <QTextEdit>
#include <QMessageBox>
#include <QCloseEvent>
#include <QFileSystemWatcher>

#include "lq3dView.h"
#include "MruHelper.h"
#include "RowColIndicators.h"
#include "ParenMatching.h"

/** display a single graph (dot file)
  * - lq3DView main viewer
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

public slots:

};

#endif // MAINWINDOW_H
