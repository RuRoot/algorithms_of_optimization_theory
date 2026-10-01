#include <cstdint>
#include <cctype>
#include <string>
#include <deque>
#include <vector>
#include <cmath>
#include <iostream>
#include <algorithm>

using namespace std;

class Variable_factor {
private:
	unsigned int index;
	double power;
	vector<Variable_factor*> coef;

public:
	Variable_factor(): index(0), power(0.0) {}

	Variable_factor(unsigned int idx, double p): index(idx), power(p) {}

	Variable_factor(const string& input);
	~Variable_factor() = default;

	unsigned int get_index() const{
		return index;
	}

	double get_power() const{
		return power;
	}

	void set_power(double p){
		power = p;
	}

	vector<Variable_factor*>& get_coef(){
		return coef;
	}

	Variable_factor* get_numerical_coef() const{
		if (coef.empty()){
			return nullptr;
		}
		return coef[0];
	}

	void set_numerical_coef(Variable_factor* c){
		if (coef.empty()){
			coef.push_back(c);
		}
		else{
			coef[0] = c;
		}
	}

	static void attach_numerical_coef(deque<Variable_factor*>& monomial){
		Variable_factor* coeff = nullptr;
		for (auto* t : monomial){
			if (t->get_index() == 0){
				coeff = t;
				break;
			}
		}
		if (!coeff){
			coeff = new Variable_factor(0, 1.0);
			monomial.push_front(coeff);
		}
		for (auto* t : monomial){
			t->set_numerical_coef(coeff);
		}
	}

	static void clear_monomial(deque<Variable_factor*>& monomial){
		for (auto* p : monomial){
			delete p;
		}
		monomial.clear();
	}
};

Variable_factor::Variable_factor(const string& s): index(0), power(1.0){
	size_t i = 0;
	size_t n = s.size();
	while (i < n && isspace((unsigned char)s[i])){
		++i;
	}
	if (i >= n){
		return;
	}

	int sign = 1;
	if (s[i] == '+' || s[i] == '-'){
		if (s[i] == '-'){
			sign = -1;
		}
		++i;
	}
	while (i < n && isspace((unsigned char)s[i])){
		++i;
	}

	size_t start = i;
	while (i < n && (isdigit((unsigned char)s[i]) || s[i] == '.')){
		++i;
	}
	if (i > start){
		index = 0;
		power = sign * stod(s.substr(start, i - start));
		return;
	}

	if (i < n && (s[i] == 'x' || s[i] == 'X')){
		++i;
		size_t dstart = i;
		while (i < n && isdigit((unsigned char)s[i])){
			++i;
		}
		if (i > dstart){
			index = (unsigned int)stoul(s.substr(dstart, i - dstart));
		}
		else{
			index = 1;
		}

		if (i < n && s[i] == '^'){
			++i;
			power = stod(s.substr(i));
		}
		else{
			power = 1.0;
		}
	}
}

bool differentiate(deque<Variable_factor*>& monomial, unsigned int diff_var_index){
	Variable_factor* target = nullptr;
	for (auto* t : monomial){
		if (t->get_index() == diff_var_index){
			target = t;
			break;
		}
	}

	if (!target || target->get_power() == 0.0){
		Variable_factor::clear_monomial(monomial);
		monomial.push_back(new Variable_factor(0, 0.0));
		return false;
	}

	double old_power = target->get_power();
	target->set_power(old_power - 1.0);

	Variable_factor* coeff = target->get_numerical_coef();
	if (!coeff){
		Variable_factor::attach_numerical_coef(monomial);
		coeff = target->get_numerical_coef();
	}
	coeff->set_power(coeff->get_power() * old_power);
	return true;
}

using Monomial = deque<Variable_factor*>;
using Polynomial = vector<Monomial>;

Monomial parse_monomial(const string& s){
	Monomial m;
	size_t i = 0;
	size_t n = s.size();

	while (i < n && isspace((unsigned char)s[i])){
		++i;
	}
	if (i >= n){
		return m;
	}

	int sign = 1;
	if (s[i] == '+' || s[i] == '-'){
		if (s[i] == '-'){
			sign = -1;
		}
		++i;
	}
	while (i < n && isspace((unsigned char)s[i])){
		++i;
	}

	double coeff = 1.0;
	size_t cstart = i;
	while (i < n && (isdigit((unsigned char)s[i]) || s[i] == '.')){
		++i;
	}
	if (i > cstart){
		coeff = stod(s.substr(cstart, i - cstart));
	}
	coeff *= sign;

	Variable_factor* cf = new Variable_factor(0, coeff);
	m.push_back(cf);

	while (i < n){
		while (i < n && isspace((unsigned char)s[i])){
			++i;
		}
		if (i >= n){
			break;
		}

		if (s[i] == '*'){
			++i;
			continue;
		}

		if (s[i] == 'x' || s[i] == 'X'){
			++i;
			size_t dstart = i;
			while (i < n && isdigit((unsigned char)s[i])){
				++i;
			}
			unsigned int idx = 1;
			if (i > dstart){
				idx = (unsigned int)stoul(s.substr(dstart, i - dstart));
			}

			double p = 1.0;
			if (i < n && s[i] == '^'){
				++i;
				size_t pstart = i;
				if (i < n && (s[i] == '+' || s[i] == '-')){
					++i;
				}
				while (i < n && (isdigit((unsigned char)s[i]) || s[i] == '.')){
					++i;
				}
				p = stod(s.substr(pstart, i - pstart));
			}
			m.push_back(new Variable_factor(idx, p));
		}
		else{
			++i;
		}
	}

	Variable_factor::attach_numerical_coef(m);
	return m;
}

Polynomial parse_polynomial(const string& s)
{
	Polynomial poly;
	string current;

	auto flush = [&](){
		bool has = false;
		for (char c : current){
			if (!isspace((unsigned char)c))
			{
				has = true;
				break;
			}
		}
		if (has){
			poly.push_back(parse_monomial(current));
		}
		current.clear();
	};

	for (size_t i = 0; i < s.size(); ++i){
		char c = s[i];
		if ((c == '+' || c == '-') && !current.empty()){
			size_t k = current.size();
			while (k > 0 && isspace((unsigned char)current[k - 1])){
				--k;
			}
			if (k > 0 && (current[k - 1] == '^' || current[k - 1] == '*' ||
					current[k - 1] == 'e' || current[k - 1] == 'E')){
				current += c;
				continue;
			}
			flush();
		}
		current += c;
	}
	flush();
	return poly;
}

Monomial clone_monomial(const Monomial& src){
	Monomial copy;
	Variable_factor* coeff = nullptr;
	for (auto* t : src){
		Variable_factor* n = new Variable_factor(t->get_index(), t->get_power());
		copy.push_back(n);
		if (t->get_index() == 0){
			coeff = n;
		}
	}
	if (!coeff){
		coeff = new Variable_factor(0, 1.0);
		copy.push_front(coeff);
	}
	for (auto* t : copy){
		t->set_numerical_coef(coeff);
	}
	return copy;
}

Polynomial clone_polynomial(const Polynomial& p){
	Polynomial r;
	for (const auto& m : p){
		r.push_back(clone_monomial(m));
	}
	return r;
}

bool is_zero_monomial(const Monomial& m){
	if (m.size() != 1){
		return false;
	}
	if (m[0]->get_index() != 0){
		return false;
	}
	return m[0]->get_power() == 0.0;
}

Polynomial differentiate_poly(const Polynomial& p, unsigned int var){
	Polynomial result;
	for (const auto& m : p){
		Monomial copy = clone_monomial(m);
		differentiate(copy, var);
		if (is_zero_monomial(copy)){
			Variable_factor::clear_monomial(copy);
			continue;
		}
		result.push_back(move(copy));
	}
	if (result.empty()){
		Monomial zero;
		zero.push_back(new Variable_factor(0, 0.0));
		result.push_back(zero);
	}
	return result;
}

void substitution_monomial(Monomial& m, const vector<double>& values){
	Variable_factor* coeff = nullptr;
	for (auto* t : m){
		if (t->get_index() == 0){
			coeff = t;
			break;
		}
	}
	if (!coeff){
		coeff = new Variable_factor(0, 1.0);
		m.push_front(coeff);
	}

	deque<Variable_factor*> to_remove;
	for (auto* t : m){
		if (t->get_index() == 0){
			continue;
		}
		unsigned int idx = t->get_index();
		if (idx < values.size()){
			coeff->set_power(coeff->get_power() * pow(values[idx], t->get_power()));
			to_remove.push_back(t);
		}
	}
	for (auto* t : to_remove){
		for (auto it = m.begin(); it != m.end(); ++it){
			if (*it == t){
				m.erase(it);
				break;
			}
		}
		delete t;
	}
}

void substitution(Polynomial& p, const vector<double>& values){
	for (auto& m : p){
		substitution_monomial(m, values);
	}
}

void simplify(Polynomial& poly){
	for (auto& m : poly){
		deque<Variable_factor*> to_remove;
		for (auto* t : m){
			if (t->get_index() != 0 && t->get_power() == 0.0){
				to_remove.push_back(t);
			}
		}
		for (auto* t : to_remove){
			for (auto it = m.begin(); it != m.end(); ++it){
				if (*it == t){
					m.erase(it);
					break;
				}
			}
			delete t;
		}

		for (size_t i = 0; i < m.size(); ++i){
			if (m[i]->get_index() == 0){
				continue;
			}
			for (size_t j = i + 1; j < m.size(); )
			{
				if (m[j]->get_index() == m[i]->get_index()){
					m[i]->set_power(m[i]->get_power() + m[j]->get_power());
					delete m[j];
					m.erase(m.begin() + j);
				}
				else{
					++j;
				}
			}
		}
	}

	Polynomial result;
	vector<bool> used(poly.size(), false);

	for (size_t i = 0; i < poly.size(); ++i){
		if (used[i]){
			continue;
		}

		vector<pair<unsigned int, double>> sig_i;
		for (auto* t : poly[i]){
			if (t->get_index() != 0){
				sig_i.push_back({t->get_index(), t->get_power()});
			}
		}
		sort(sig_i.begin(), sig_i.end());

		double total = 0.0;
		Variable_factor* coeff_i = nullptr;
		for (auto* t : poly[i]){
			if (t->get_index() == 0){
				coeff_i = t;
				break;
			}
		}
		if (coeff_i){
			total += coeff_i->get_power();
		}

		used[i] = true;
		for (size_t j = i + 1; j < poly.size(); ++j){
			if (used[j]){
				continue;
			}
			vector<pair<unsigned int, double>> sig_j;
			for (auto* t : poly[j]){
				if (t->get_index() != 0){
					sig_j.push_back({t->get_index(), t->get_power()});
				}
			}
			sort(sig_j.begin(), sig_j.end());
			if (sig_i != sig_j){
				continue;
			}

			Variable_factor* cj = nullptr;
			for (auto* t : poly[j]){
				if (t->get_index() == 0){
					cj = t;
					break;
				}
			}
			if (cj){
				total += cj->get_power();
			}
			used[j] = true;
			Variable_factor::clear_monomial(poly[j]);
		}

		if (total == 0.0){
			Variable_factor::clear_monomial(poly[i]);
			continue;
		}
		if (coeff_i){
			coeff_i->set_power(total);
		}
		result.push_back(move(poly[i]));
	}

	if (result.empty()){
		Monomial zero;
		zero.push_back(new Variable_factor(0, 0.0));
		result.push_back(zero);
	}
	poly = move(result);
}

double evaluate(const Polynomial& p, const vector<double>& point){
	vector<double> values(point.size() + 1, 0.0);
	for (size_t i = 0; i < point.size(); ++i){
		values[i + 1] = point[i];
	}

	Polynomial copy = clone_polynomial(p);
	substitution(copy, values);
	simplify(copy);

	double sum = 0.0;
	for (auto& m : copy){
		for (auto* t : m){
			if (t->get_index() == 0){
				sum += t->get_power();
				break;
			}
		}
		Variable_factor::clear_monomial(m);
	}
	return sum;
}

vector<Polynomial> gradient(const Polynomial& p, unsigned int dim){
	vector<Polynomial> g(dim + 1);
	for (unsigned int i = 1; i <= dim; ++i){
		g[i] = differentiate_poly(p, i);
	}
	return g;
}

vector<double> operator-(const vector<double>& l, const vector<double>& r){
	vector<double> result(l.size());
	for (size_t i = 0; i < l.size(); ++i){
		result[i] = l[i] - r[i];
	}
	return result;
}

vector<double> operator+(const vector<double>& l, const vector<double>& r){
	vector<double> result(l.size());
	for (size_t i = 0; i < l.size(); ++i){
		result[i] = l[i] + r[i];
	}
	return result;
}

vector<double> operator*(double k, const vector<double>& v)
{
	vector<double> result(v.size());
	for (size_t i = 0; i < v.size(); ++i){
		result[i] = k * v[i];
	}
	return result;
}

vector<double> operator*(const vector<double>& v, double k)
{
	return k * v;
}

double operator*(const vector<double>& l, const vector<double>& r)
{
	double sum = 0.0;
	for (size_t i = 0; i < l.size(); ++i){
		sum += l[i] * r[i];
	}
	return sum;
}

double norm(const vector<double>& v)
{
	return sqrt(v * v);
}

vector<double> grad_at(const vector<Polynomial>& g, const vector<double>& point)
{
	unsigned int dim = (unsigned int)point.size();
	vector<double> result(dim);
	for (unsigned int i = 1; i <= dim; ++i){
		result[i - 1] = evaluate(g[i], point);
	}
	return result;
}

vector<double> find_min(const Polynomial& f,
						const vector<Polynomial>& g,
						double error = 0.01,
						vector<double> current_point = {0, 0, 0},
						double current_step = 0.01,
						int quant_iteration = 100)
{
	unsigned int dim = (unsigned int)current_point.size();
	vector<double> direction = (-1.0) * grad_at(g, current_point);

	while (quant_iteration > 0){
		vector<double> next_point = current_point + current_step * direction;

		if (norm(next_point - current_point) <= error){
			break;
		}

		if (evaluate(f, current_point) > evaluate(f, next_point)){
			current_point = next_point;
			direction = (-1.0) * grad_at(g, current_point);
		}
		else{
			current_step *= 0.5;
		}

		quant_iteration--;
	}
	return current_point;
}

int main(){
	Polynomial f = parse_polynomial(
		"5x1^2 - 2x2^2 - 2x3^2 + 2x1x2 - x2x3 + 7x2"
	);

	unsigned int dim = 3;
	vector<Polynomial> g = gradient(f, dim);

	vector<double> start = {-10, -10, -10};
	vector<double> min_point = find_min(f, g, 0.01, start, 0.01, 10000);

	cout << "x1 = " << min_point[0] << endl;
	cout << "x2 = " << min_point[1] << endl;
	cout << "x3 = " << min_point[2] << endl;
	cout << "f(x) = " << evaluate(f, min_point) << endl;

	return 0;
}
