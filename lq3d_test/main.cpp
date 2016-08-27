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

#include <QApplication>
#include "mainwindow.h"
#include "lq3d.h"
#include "lq3dView.h"
#include "lq3d_configure.h"

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    if (1) {
        auto c_view = new lq3dView;
        QWidget *container = QWidget::createWindowContainer(c_view);
        c_view->cylinderTest();
        container->show();
        return app.exec();
    }

    if (0) {
        lq3dView c_view, t_view;
        c_view.cylinderTest();
        t_view.torusTest();
        c_view.show();
        t_view.show();
        return app.exec();
    }

    MainWindow w(argc, argv);
    w.show();
    return app.exec();
}
