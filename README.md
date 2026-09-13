<h1 align="left">Course Difficulty and Academic Performance Moderate the Relationship Between Procrastination and Grades</h1>

<h2>Introduction</h2>

<p>
This repository provides the data preprocessing and analysis code for the paper 
<i>"Course Difficulty and Academic Performance Moderate the Relationship Between Procrastination and Grades."</i>
The repository documents the construction of the analytic sample, IRT-based estimation of course difficulty and academic performance propensity, primary hierarchical linear model analyses, interaction analyses, model diagnostics, descriptive analyses, and robustness and sensitivity checks.
</p>


<h2>Repository Structure</h2>

<ul>
  <li>
    <b><code>preprocessing.R</code></b>: 
    Data preprocessing, sample construction, and IRT-based estimation of course difficulty and academic performance propensity.
  </li>

  <li>
    <b><code>main_analysis.R</code></b>: 
    Primary model estimation, cluster-robust inference, model diagnostics, interaction analyses, descriptive analyses, and additional robustness checks.
  </li>

  <li>
    <b><code>robustness_diff_operationalization.R</code></b>: 
    Robustness checks using alternative operationalizations of procrastination and submission timing.
  </li>

  <li>
    <b><code>robustness_irt_diff_cutoffs_prior_semesters.R</code></b>: 
    Robustness checks using alternative IRT grade cutoffs and prior-semester calibration.
  </li>
</ul>

<h2>Data Preprocessing</h2>
<p>The preprocessing pipeline includes:</p>
<ul>
  <li><b>Compute procrastination index</b>: Constructed a rank-based index of submission delays within each course–term.</li>
  <li><b>Filter by grading scheme</b>: Excluded courses graded on a Pass/Fail basis and enrollments without final letter grades (A–F).</li>
  <li><b>Restrict to sufficiently large majors</b>
    <ul>
      <li>≥ 20 distinct courses</li>
      <li>≥ 100 enrollments</li>
      <li>Top 10% by average enrollment per course</li>
    </ul>
  </li>
  <li><b>Drop failed IRT* cases</b>: Removed majors where IRT estimation failed due to data sparsity.</li>
  <li><b>Remove multidimensional majors</b>: Dropped majors showing evidence of multidimensionality.</li>
</ul>
<p>
*We performed the IRT estimation following the procedure provided in 
<a href="https://dl.acm.org/doi/abs/10.1145/3657604.3662028">Baucks et al. (2024)</a>, 
whose replication code is publicly available on 
<a href="https://github.com/your-repo-link">GitHub</a>.
</p>

<h2>Data Analysis</h2>
<p>
The analysis builds on the final analytic dataset and includes the following procedures:
</p>
<ul>
  <li>
    <b>Standardization</b><br>
    Course grade, procrastination index, course difficulty, and academic performance propensity were standardized to have mean 0 and standard deviation 1 for the primary model analyses.
  </li>
  <li><b>Hierarchical Linear Models (HLMs)</b><br>
      Seven nested models were estimated using <code>lmer</code>:
      <ol>
        <li>Course grade ~ Procrastination + (1 | Student)</li>
        <li>Course grade ~ Procrastination + Course difficulty + (1 | Student)</li>
        <li>Course grade ~ Procrastination + Course difficulty + Academic Performance Propensity + (1 | Student)</li>
        <li>Course grade ~ Procrastination × Course difficulty + Academic Performance Propensity + (1 | Student)</li>
        <li>Course grade ~ Procrastination × (Course difficulty + Academic Performance Propensity) + (1 | Student)</li>
        <li>Course grade ~ Procrastination × (Course difficulty + Academic Performance Propensity) + difficulty × Academic Performance Propensity + (1 | Student)</li>
        <li>Course grade ~ Procrastination × Course difficulty × Academic Performance Propensity + (1 | Student)</li>
      </ol>
  </li>

 <li>
    <b>Cluster-robust inference</b><br>
    Cluster-robust variance estimators (CR2) were used for statistical inference in the primary models.
  </li>

  <li>
    <b>Model comparison</b><br>
    Nested models were compared using likelihood ratio tests.
  </li>

  <li>
    <b>Model diagnostics</b><br>
    Standard residual and influence diagnostics were conducted to evaluate distributional and modeling assumptions.
  </li>

  <li>
    <b>Interaction analyses</b><br>
    Two-way and three-way interaction effects were examined using predicted course grades, visualization, and formal simple-slope analyses.
  </li>

  <li>
    <b>Descriptive analyses</b><br>
    Descriptive characteristics of the analytic sample and comparisons between retained and excluded observations were examined.
  </li>

  <li>
    <b>Robustness and sensitivity analyses</b><br>
    The robustness of the findings was assessed using alternative outcome models, random- and fixed-effect specifications, disciplinary subgroup analyses, demographic adjustments, alternative IRT calibrations, sample restrictions, and alternative operationalizations of procrastination.
  </li>

</ul>


<h2>Robustness Analysis Files</h2>

<p>
Additional robustness analyses are organized into two separate scripts:
</p>

<ul>

  <li>
    <b><code>robustness_diff_operationalization.R</code></b><br>
    Examines alternative submission-delay measures, the exclusion of non-submissions, and sensitivity to extreme submission-delay values.
  </li>

  <li>
    <b><code>robustness_irt_diff_cutoffs_prior_semesters.R</code></b><br>
    Examines alternative grade cutoffs for IRT estimation and the estimation of course difficulty and academic performance propensity using prior-semester grade information.
  </li>

</ul>
