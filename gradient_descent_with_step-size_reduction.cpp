#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

double function(const vector<double>& param) {
    double f = 5 * param[0] * param[0]
             - 2 * param[1] * param[1]
             - 2 * param[2] * param[2]
             + 2 * param[0] * param[1]
             - param[1] * param[2]
             + 7 * param[1];
    return f;
}

vector<double> grad(const vector<double>& current_point) {
    double diff_x1 = 10 * current_point[0] + 2 * current_point[1];
    double diff_x2 = -4 * current_point[1] + 2 * current_point[0] - current_point[2] + 7;
    double diff_x3 = -4 * current_point[2] - current_point[1];

    vector<double> output = {diff_x1, diff_x2, diff_x3};
    return output;
}

vector<double> operator-(const vector<double>& l, const vector<double>& r) {
    vector<double> result(3);
    for (int i = 0; i < 3; ++i) {
        result[i] = l[i] - r[i];
    }
    return result;
}

vector<double> operator+(const vector<double>& l, const vector<double>& r) {
    vector<double> result(3);
    for (int i = 0; i < 3; ++i) {
        result[i] = l[i] + r[i];
    }
    return result;
}

vector<double> operator*(double k, const vector<double>& v) {
    vector<double> result(3);
    for (int i = 0; i < 3; ++i) {
        result[i] = k * v[i];
    }
    return result;
}

vector<double> operator*(const vector<double>& v, double k) {
    return k * v;
}

// скалярное произведение
double operator*(const vector<double>& l, const vector<double>& r) {
    double sum = 0.0;
    for (int i = 0; i < 3; ++i) {
        sum += l[i] * r[i];
    }
    return sum;
}

double norm(const vector<double>& v) {
    return sqrt(v * v);
}

vector<double> find_min(double error = 0.01,
                        vector<double> current_point = {0, 0, 0},
                        double current_step = 0.01,
                        int quant_iteration = 100,
                        double crushing_ratio = 0.5)
{
    vector<double> current_step_vector = (-1.0) * grad(current_point);

    while (quant_iteration > 0) {
        vector<double> next_point = current_point + current_step * current_step_vector;

        // Останавливаемся, если шаг стал слишком маленьким
        if (norm(next_point - current_point) <= error) {
            break;
        }

        if (function(current_point) > function(next_point)) {
            current_point = next_point;
            current_step_vector = (-1.0) * grad(current_point);
        } else {
            current_step = current_step * crushing_ratio;
        }

        quant_iteration--;
    }

    return current_point;
}

int main() {
    vector<double> start = {-10, -10, -10};
    vector<double> min_point = find_min(0.01, start, 0.01, 10000, 0.5);

    cout << "x1 = " << min_point[0] << endl;
    cout << "x2 = " << min_point[1] << endl;
    cout << "x3 = " << min_point[2] << endl;
    cout << "f(x) = " << function(min_point) << endl;

    return 0;
}
