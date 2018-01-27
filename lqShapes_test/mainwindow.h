/*
    lqShapes_test: SWI-Prolog and Qt Graphics Framework

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

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QSplitter>
#include <QTextEdit>
#include <QMainWindow>
#include <QStackedWidget>

#include "ConsoleEdit.h"
#include "lqShapesView.h"

/**
 *  @brief The MainWindow class
 *  setup overall layout:
 *  - commands
 *  - a large graphic view
 *  - overlapped views on script,console
*/
class MainWindow : public QMainWindow
{
    Q_OBJECT

    QSplitter* splitter() const;
    QStackedWidget* stacked() const;

public:

    MainWindow(int argc, char **argv, QWidget *parent = 0);

    lqShapesView* view() const;
    ConsoleEdit* console() const;
    QTextEdit* script() const;

    void setScript(QString file);
};

#endif // MAINWINDOW_H
