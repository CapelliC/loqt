/*
    pqConsole    : interfacing SWI-Prolog and Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright @ 2023

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

#include "pqProofView.h"
#include <qmath.h>

/**
 * @brief pqProofView::pqProofView
 *  preset handy defaults
 */
pqProofView::pqProofView()
{
    setRenderHint(QPainter::Antialiasing);
    setRenderHint(QPainter::TextAntialiasing);
    setTransformationAnchor(AnchorUnderMouse);
    setResizeAnchor(AnchorUnderMouse);
    setDragMode(ScrollHandDrag);
}

pqProofView::~pqProofView()
{

}

/**
 * @brief pqProofView::wheelEvent
 *  zooming by scroll wheel
 * @param event
 */
void pqProofView::wheelEvent(QWheelEvent *event)
{
    Q_ASSERT(false);
    /*
    qreal factor = qPow(1.2, event->delta() / 240.0);
    scale(factor, factor);
    */
    event->accept();
}

/**
 * @brief pqProofView::scale_view
 *  adjust scaling of current view
 * @param scaleFactor
 *  fractional scale amount
 */
void pqProofView::scale_view(qreal scaleFactor)
{
    qreal f = sqrt(transform().determinant());

    if (scaleFactor * f > 8.0)
        scaleFactor = 8.0 / f;
    if (scaleFactor * f < 0.1)
        scaleFactor = 0.1 / f;

    scale(scaleFactor, scaleFactor);
}
