"""Modelo de regresion lineal por minimos cuadrados ordinarios.

Este modulo define una clase pequena y autocontenida para ajustar modelos de
regresion lineal sin depender de scikit-learn.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np


@dataclass
class ResultadoAjuste:
    """Resumen numerico del ajuste de un modelo lineal."""

    coeficientes: np.ndarray
    intercepto: float
    r2: float
    mse: float
    rmse: float
    rank: int


class ModeloRegresionLineal:
    """Regresion lineal estimada por minimos cuadrados ordinarios.

    Parameters
    ----------
    incluir_intercepto:
        Si es True, agrega una columna de unos a la matriz de diseno para
        estimar el intercepto del modelo.
    """

    def __init__(self, incluir_intercepto: bool = True) -> None:
        self.incluir_intercepto = incluir_intercepto
        self.coeficientes_: np.ndarray | None = None
        self.intercepto_: float | None = None
        self.residuos_: np.ndarray | None = None
        self.valores_ajustados_: np.ndarray | None = None
        self.r2_: float | None = None
        self.mse_: float | None = None
        self.rmse_: float | None = None
        self.rank_: int | None = None

    def ajustar(self, X: np.ndarray, y: np.ndarray) -> ResultadoAjuste:
        """Ajusta el modelo lineal.

        Parameters
        ----------
        X:
            Matriz de variables explicativas con forma (n_observaciones,
            n_variables). Si se pasa un vector, se interpreta como una sola
            variable explicativa.
        y:
            Vector de respuesta con n_observaciones valores.

        Returns
        -------
        ResultadoAjuste
            Objeto con coeficientes y metricas principales del ajuste.
        """

        X_validado = self._validar_X(X)
        y_validado = self._validar_y(y, X_validado.shape[0])
        X_diseno = self._agregar_intercepto(X_validado)

        beta, _, rank, _ = np.linalg.lstsq(X_diseno, y_validado, rcond=None)

        predicciones = X_diseno @ beta
        residuos = y_validado - predicciones
        mse = float(np.mean(residuos**2))
        rmse = float(np.sqrt(mse))
        r2 = self._calcular_r2(y_validado, predicciones)

        if self.incluir_intercepto:
            self.intercepto_ = float(beta[0])
            self.coeficientes_ = beta[1:]
        else:
            self.intercepto_ = 0.0
            self.coeficientes_ = beta

        self.valores_ajustados_ = predicciones
        self.residuos_ = residuos
        self.r2_ = r2
        self.mse_ = mse
        self.rmse_ = rmse
        self.rank_ = int(rank)

        return ResultadoAjuste(
            coeficientes=self.coeficientes_.copy(),
            intercepto=self.intercepto_,
            r2=self.r2_,
            mse=self.mse_,
            rmse=self.rmse_,
            rank=self.rank_,
        )

    def predecir(self, X: np.ndarray) -> np.ndarray:
        """Predice valores de respuesta para nuevas observaciones."""

        self._verificar_ajustado()
        X_validado = self._validar_X(X)

        if X_validado.shape[1] != self.coeficientes_.shape[0]:
            raise ValueError(
                "X debe tener el mismo numero de variables usadas en el ajuste: "
                f"{self.coeficientes_.shape[0]}."
            )

        return self.intercepto_ + X_validado @ self.coeficientes_

    def puntaje_r2(self, X: np.ndarray, y: np.ndarray) -> float:
        """Calcula R^2 para datos dados."""

        predicciones = self.predecir(X)
        y_validado = self._validar_y(y, predicciones.shape[0])
        return self._calcular_r2(y_validado, predicciones)

    def resumen(self) -> str:
        """Devuelve un resumen legible del modelo ajustado."""

        self._verificar_ajustado()
        lineas = [
            "Modelo de regresion lineal",
            f"Intercepto: {self.intercepto_:.6f}",
            f"Coeficientes: {np.array2string(self.coeficientes_, precision=6)}",
            f"R^2: {self.r2_:.6f}",
            f"MSE: {self.mse_:.6f}",
            f"RMSE: {self.rmse_:.6f}",
            f"Rango de la matriz de diseno: {self.rank_}",
        ]
        return "\n".join(lineas)

    def _agregar_intercepto(self, X: np.ndarray) -> np.ndarray:
        if not self.incluir_intercepto:
            return X

        unos = np.ones((X.shape[0], 1))
        return np.column_stack((unos, X))

    @staticmethod
    def _validar_X(X: np.ndarray) -> np.ndarray:
        X_validado = np.asarray(X, dtype=float)

        if X_validado.ndim == 1:
            X_validado = X_validado.reshape(-1, 1)

        if X_validado.ndim != 2:
            raise ValueError("X debe ser un vector o una matriz bidimensional.")

        if X_validado.shape[0] == 0:
            raise ValueError("X debe contener al menos una observacion.")

        if not np.isfinite(X_validado).all():
            raise ValueError("X no debe contener valores NaN o infinitos.")

        return X_validado

    @staticmethod
    def _validar_y(y: np.ndarray, n_observaciones: int) -> np.ndarray:
        y_validado = np.asarray(y, dtype=float).reshape(-1)

        if y_validado.shape[0] != n_observaciones:
            raise ValueError(
                "y debe tener el mismo numero de observaciones que X: "
                f"{n_observaciones}."
            )

        if not np.isfinite(y_validado).all():
            raise ValueError("y no debe contener valores NaN o infinitos.")

        return y_validado

    @staticmethod
    def _calcular_r2(y: np.ndarray, predicciones: np.ndarray) -> float:
        suma_cuadrados_residual = float(np.sum((y - predicciones) ** 2))
        suma_cuadrados_total = float(np.sum((y - np.mean(y)) ** 2))

        if suma_cuadrados_total == 0.0:
            return 1.0 if suma_cuadrados_residual == 0.0 else 0.0

        return 1.0 - suma_cuadrados_residual / suma_cuadrados_total

    def _verificar_ajustado(self) -> None:
        if self.coeficientes_ is None or self.intercepto_ is None:
            raise RuntimeError("Primero debes ajustar el modelo con ajustar(X, y).")


if __name__ == "__main__":
    X_ejemplo = np.array(
        [
            [1.0, 2.0],
            [2.0, 1.0],
            [3.0, 4.0],
            [4.0, 3.0],
            [5.0, 5.0],
        ]
    )
    y_ejemplo = np.array([5.0, 6.0, 11.0, 12.0, 16.0])

    modelo = ModeloRegresionLineal()
    modelo.ajustar(X_ejemplo, y_ejemplo)

    print(modelo.resumen())
    print("Predicciones:", modelo.predecir(X_ejemplo))
