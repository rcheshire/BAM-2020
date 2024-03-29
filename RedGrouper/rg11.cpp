  #include "admodel.h"          // Include AD class definitions
  #include "admb2r.cpp"         // Include S-compatible output functions (needs preceding)
  #include <time.h>
	time_t start,finish;
	long hour,minute,second;	
	double elapsed_time;
	
#include <admodel.h>
#include <contrib.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <rg11.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
cout << "Starting Beaufort Assessment Model" << endl;
cout << endl;
cout << "                BAM!" << endl;
cout << endl;
  styr.allocate("styr");
  endyr.allocate("endyr");
  styr_rec_dev.allocate("styr_rec_dev");
  endyr_rec_dev.allocate("endyr_rec_dev");
  endyr_rec_phase1.allocate("endyr_rec_phase1");
  endyr_rec_phase2.allocate("endyr_rec_phase2");
  endyr_selex_phase1.allocate("endyr_selex_phase1");
  endyr_selex_phase2.allocate("endyr_selex_phase2");
  sizelim1.allocate("sizelim1");
  sizelim2.allocate("sizelim2");
   nyrs=endyr-styr+1.;
   nyrs_rec=endyr_rec_dev-styr_rec_dev+1.;
  nages.allocate("nages");
  agebins.allocate(1,nages,"agebins");
  nages_agec.allocate("nages_agec");
  agebins_agec.allocate(1,nages_agec,"agebins_agec");
  nlenbins.allocate("nlenbins");
  lenbins_width.allocate("lenbins_width");
  lenbins.allocate(1,nlenbins,"lenbins");
  max_F_spr_msy.allocate("max_F_spr_msy");
  n_iter_spr.allocate("n_iter_spr");
		n_iter_msy=n_iter_spr; 
  styr_rec_spr.allocate("styr_rec_spr");
  endyr_rec_spr.allocate("endyr_rec_spr");
   nyrs_rec_spr=endyr_rec_spr-styr_rec_spr+1.;
  selpar_n_yrs_wgted.allocate("selpar_n_yrs_wgted");
  set_BiasCor.allocate("set_BiasCor");
  styr_CVT_cpue.allocate("styr_CVT_cpue");
  endyr_CVT_cpue.allocate("endyr_CVT_cpue");
  obs_CVT_cpue.allocate(styr_CVT_cpue,endyr_CVT_cpue,"obs_CVT_cpue");
  CVT_cpue_cv.allocate(styr_CVT_cpue,endyr_CVT_cpue,"CVT_cpue_cv");
  nyr_CVT_lenc.allocate("nyr_CVT_lenc");
  yrs_CVT_lenc.allocate(1,nyr_CVT_lenc,"yrs_CVT_lenc");
  nsamp_CVT_lenc.allocate(1,nyr_CVT_lenc,"nsamp_CVT_lenc");
  nfish_CVT_lenc.allocate(1,nyr_CVT_lenc,"nfish_CVT_lenc");
  obs_CVT_lenc.allocate(1,nyr_CVT_lenc,1,nlenbins,"obs_CVT_lenc");
  nyr_CVT_agec.allocate("nyr_CVT_agec");
  yrs_CVT_agec.allocate(1,nyr_CVT_agec,"yrs_CVT_agec");
  nsamp_CVT_agec.allocate(1,nyr_CVT_agec,"nsamp_CVT_agec");
  nfish_CVT_agec.allocate(1,nyr_CVT_agec,"nfish_CVT_agec");
  obs_CVT_agec.allocate(1,nyr_CVT_agec,1,nages_agec,"obs_CVT_agec");
  styr_cH_cpue.allocate("styr_cH_cpue");
  endyr_cH_cpue.allocate("endyr_cH_cpue");
  obs_cH_cpue.allocate(styr_cH_cpue,endyr_cH_cpue,"obs_cH_cpue");
  cH_cpue_cv.allocate(styr_cH_cpue,endyr_cH_cpue,"cH_cpue_cv");
  styr_cH_L.allocate("styr_cH_L");
  endyr_cH_L.allocate("endyr_cH_L");
  obs_cH_L.allocate(styr_cH_L,endyr_cH_L,"obs_cH_L");
  cH_L_cv.allocate(styr_cH_L,endyr_cH_L,"cH_L_cv");
  nyr_cH_lenc.allocate("nyr_cH_lenc");
  yrs_cH_lenc.allocate(1,nyr_cH_lenc,"yrs_cH_lenc");
  nsamp_cH_lenc.allocate(1,nyr_cH_lenc,"nsamp_cH_lenc");
  nfish_cH_lenc.allocate(1,nyr_cH_lenc,"nfish_cH_lenc");
  obs_cH_lenc.allocate(1,nyr_cH_lenc,1,nlenbins,"obs_cH_lenc");
  nyr_cH_agec.allocate("nyr_cH_agec");
  yrs_cH_agec.allocate(1,nyr_cH_agec,"yrs_cH_agec");
  nsamp_cH_agec.allocate(1,nyr_cH_agec,"nsamp_cH_agec");
  nfish_cH_agec.allocate(1,nyr_cH_agec,"nfish_cH_agec");
  obs_cH_agec.allocate(1,nyr_cH_agec,1,nages_agec,"obs_cH_agec");
  styr_cH_D.allocate("styr_cH_D");
  endyr_cH_D.allocate("endyr_cH_D");
  obs_cH_released.allocate(styr_cH_D,endyr_cH_D,"obs_cH_released");
  cH_D_cv.allocate(styr_cH_D,endyr_cH_D,"cH_D_cv");
  styr_cO_L.allocate("styr_cO_L");
  endyr_cO_L.allocate("endyr_cO_L");
  obs_cO_L.allocate(styr_cO_L,endyr_cO_L,"obs_cO_L");
  cO_L_cv.allocate(styr_cO_L,endyr_cO_L,"cO_L_cv");
  nyr_cO_lenc.allocate("nyr_cO_lenc");
  yrs_cO_lenc.allocate(1,nyr_cO_lenc,"yrs_cO_lenc");
  nsamp_cO_lenc.allocate(1,nyr_cO_lenc,"nsamp_cO_lenc");
  nfish_cO_lenc.allocate(1,nyr_cO_lenc,"nfish_cO_lenc");
  obs_cO_lenc.allocate(1,nyr_cO_lenc,1,nlenbins,"obs_cO_lenc");
  styr_HB_cpue.allocate("styr_HB_cpue");
  endyr_HB_cpue.allocate("endyr_HB_cpue");
  obs_HB_cpue.allocate(styr_HB_cpue,endyr_HB_cpue,"obs_HB_cpue");
  HB_cpue_cv.allocate(styr_HB_cpue,endyr_HB_cpue,"HB_cpue_cv");
  styr_HB_L.allocate("styr_HB_L");
  endyr_HB_L.allocate("endyr_HB_L");
  obs_HB_L.allocate(styr_HB_L,endyr_HB_L,"obs_HB_L");
  HB_L_cv.allocate(styr_HB_L,endyr_HB_L,"HB_L_cv");
  nyr_HB_lenc.allocate("nyr_HB_lenc");
  yrs_HB_lenc.allocate(1,nyr_HB_lenc,"yrs_HB_lenc");
  nsamp_HB_lenc.allocate(1,nyr_HB_lenc,"nsamp_HB_lenc");
  nfish_HB_lenc.allocate(1,nyr_HB_lenc,"nfish_HB_lenc");
  obs_HB_lenc.allocate(1,nyr_HB_lenc,1,nlenbins,"obs_HB_lenc");
  nyr_HB_agec.allocate("nyr_HB_agec");
  yrs_HB_agec.allocate(1,nyr_HB_agec,"yrs_HB_agec");
  nsamp_HB_agec.allocate(1,nyr_HB_agec,"nsamp_HB_agec");
  nfish_HB_agec.allocate(1,nyr_HB_agec,"nfish_HB_agec");
  obs_HB_agec.allocate(1,nyr_HB_agec,1,nages_agec,"obs_HB_agec");
  styr_HB_D.allocate("styr_HB_D");
  endyr_HB_D.allocate("endyr_HB_D");
  obs_HB_released.allocate(styr_HB_D,endyr_HB_D,"obs_HB_released");
  HB_D_cv.allocate(styr_HB_D,endyr_HB_D,"HB_D_cv");
  nyr_HB_D_lenc.allocate("nyr_HB_D_lenc");
  yrs_HB_D_lenc.allocate(1,nyr_HB_D_lenc,"yrs_HB_D_lenc");
  nsamp_HB_D_lenc.allocate(1,nyr_HB_D_lenc,"nsamp_HB_D_lenc");
  nfish_HB_D_lenc.allocate(1,nyr_HB_D_lenc,"nfish_HB_D_lenc");
  obs_HB_D_lenc.allocate(1,nyr_HB_D_lenc,1,nlenbins,"obs_HB_D_lenc");
  styr_GR_L.allocate("styr_GR_L");
  endyr_GR_L.allocate("endyr_GR_L");
  obs_GR_L.allocate(styr_GR_L,endyr_GR_L,"obs_GR_L");
  GR_L_cv.allocate(styr_GR_L,endyr_GR_L,"GR_L_cv");
  styr_GR_D.allocate("styr_GR_D");
  endyr_GR_D.allocate("endyr_GR_D");
  obs_GR_released.allocate(styr_GR_D,endyr_GR_D,"obs_GR_released");
  GR_D_cv.allocate(styr_GR_D,endyr_GR_D,"GR_D_cv");
  nyr_GR_lenc.allocate("nyr_GR_lenc");
  yrs_GR_lenc.allocate(1,nyr_GR_lenc,"yrs_GR_lenc");
  nsamp_GR_lenc.allocate(1,nyr_GR_lenc,"nsamp_GR_lenc");
  nfish_GR_lenc.allocate(1,nyr_GR_lenc,"nfish_GR_lenc");
  obs_GR_lenc.allocate(1,nyr_GR_lenc,1,nlenbins,"obs_GR_lenc");
  nyr_GR_agec.allocate("nyr_GR_agec");
  yrs_GR_agec.allocate(1,nyr_GR_agec,"yrs_GR_agec");
  nsamp_GR_agec.allocate(1,nyr_GR_agec,"nsamp_GR_agec");
  nfish_GR_agec.allocate(1,nyr_GR_agec,"nfish_GR_agec");
  obs_GR_agec.allocate(1,nyr_GR_agec,1,nages_agec,"obs_GR_agec");
  set_Linf.allocate(1,7,"set_Linf");
  set_K.allocate(1,7,"set_K");
  set_t0.allocate(1,7,"set_t0");
  set_len_cv.allocate(1,7,"set_len_cv");
  set_M_constant.allocate(1,7,"set_M_constant");
  set_steep.allocate(1,7,"set_steep");
  set_log_R0.allocate(1,7,"set_log_R0");
  set_R_autocorr.allocate(1,7,"set_R_autocorr");
  set_rec_sigma.allocate(1,7,"set_rec_sigma");
  set_log_dm_cH_lc.allocate(1,7,"set_log_dm_cH_lc");
  set_log_dm_cO_lc.allocate(1,7,"set_log_dm_cO_lc");
  set_log_dm_HB_lc.allocate(1,7,"set_log_dm_HB_lc");
  set_log_dm_GR_lc.allocate(1,7,"set_log_dm_GR_lc");
  set_log_dm_HB_D_lc.allocate(1,7,"set_log_dm_HB_D_lc");
  set_log_dm_CVT_lc.allocate(1,7,"set_log_dm_CVT_lc");
  set_log_dm_cH_ac.allocate(1,7,"set_log_dm_cH_ac");
  set_log_dm_HB_ac.allocate(1,7,"set_log_dm_HB_ac");
  set_log_dm_GR_ac.allocate(1,7,"set_log_dm_GR_ac");
  set_log_dm_CVT_ac.allocate(1,7,"set_log_dm_CVT_ac");
  set_selpar_A50_cH2.allocate(1,7,"set_selpar_A50_cH2");
  set_selpar_slope_cH2.allocate(1,7,"set_selpar_slope_cH2");
  set_selpar_A50_cH3.allocate(1,7,"set_selpar_A50_cH3");
  set_selpar_slope_cH3.allocate(1,7,"set_selpar_slope_cH3");
  set_selpar_A50_cO2.allocate(1,7,"set_selpar_A50_cO2");
  set_selpar_A50_cO3.allocate(1,7,"set_selpar_A50_cO3");
  set_selpar_slope_cO2.allocate(1,7,"set_selpar_slope_cO2");
  set_selpar_A502_cO2.allocate(1,7,"set_selpar_A502_cO2");
  set_selpar_slope2_cO2.allocate(1,7,"set_selpar_slope2_cO2");
  set_selpar_A50_HB1.allocate(1,7,"set_selpar_A50_HB1");
  set_selpar_slope_HB1.allocate(1,7,"set_selpar_slope_HB1");
  set_selpar_A50_HB2.allocate(1,7,"set_selpar_A50_HB2");
  set_selpar_slope_HB2.allocate(1,7,"set_selpar_slope_HB2");
  set_selpar_A50_HB3.allocate(1,7,"set_selpar_A50_HB3");
  set_selpar_slope_HB3.allocate(1,7,"set_selpar_slope_HB3");
  set_selpar_A50_GR3.allocate(1,7,"set_selpar_A50_GR3");
  set_selpar_slope_GR3.allocate(1,7,"set_selpar_slope_GR3");
  set_selpar_A50_CVT.allocate(1,7,"set_selpar_A50_CVT");
  set_selpar_slope_CVT.allocate(1,7,"set_selpar_slope_CVT");
  set_selpar_A502_CVT.allocate(1,7,"set_selpar_A502_CVT");
  set_selpar_slope2_CVT.allocate(1,7,"set_selpar_slope2_CVT");
  set_selpar_age1logit_D.allocate(1,7,"set_selpar_age1logit_D");
  set_log_q_cH.allocate(1,7,"set_log_q_cH");
  set_log_q_HB.allocate(1,7,"set_log_q_HB");
  set_log_q_CVT.allocate(1,7,"set_log_q_CVT");
  set_log_avg_F_cH.allocate(1,7,"set_log_avg_F_cH");
  set_log_avg_F_cO.allocate(1,7,"set_log_avg_F_cO");
  set_log_avg_F_HB.allocate(1,7,"set_log_avg_F_HB");
  set_log_avg_F_GR.allocate(1,7,"set_log_avg_F_GR");
  set_log_avg_F_cH_D.allocate(1,7,"set_log_avg_F_cH_D");
  set_log_avg_F_HB_D.allocate(1,7,"set_log_avg_F_HB_D");
  set_log_avg_F_GR_D.allocate(1,7,"set_log_avg_F_GR_D");
  set_log_F_dev_cH.allocate(1,3,"set_log_F_dev_cH");
  set_log_F_dev_cO.allocate(1,3,"set_log_F_dev_cO");
  set_log_F_dev_HB.allocate(1,3,"set_log_F_dev_HB");
  set_log_F_dev_GR.allocate(1,3,"set_log_F_dev_GR");
  set_log_F_dev_cH_D.allocate(1,3,"set_log_F_dev_cH_D");
  set_log_F_dev_HB_D.allocate(1,3,"set_log_F_dev_HB_D");
  set_log_F_dev_GR_D.allocate(1,3,"set_log_F_dev_GR_D");
  set_log_RWq_dev.allocate(1,3,"set_log_RWq_dev");
  set_log_rec_dev.allocate(1,3,"set_log_rec_dev");
  set_log_Nage_dev.allocate(1,3,"set_log_Nage_dev");
  set_log_F_dev_cH_vals.allocate(styr_cH_L,endyr_cH_L,"set_log_F_dev_cH_vals");
  set_log_F_dev_cO_vals.allocate(styr_cO_L,endyr_cO_L,"set_log_F_dev_cO_vals");
  set_log_F_dev_HB_vals.allocate(styr_HB_L,endyr_HB_L,"set_log_F_dev_HB_vals");
  set_log_F_dev_GR_vals.allocate(styr_GR_L,endyr_GR_L,"set_log_F_dev_GR_vals");
  set_log_F_dev_cH_D_vals.allocate(styr_cH_D,endyr_cH_D,"set_log_F_dev_cH_D_vals");
  set_log_F_dev_HB_D_vals.allocate(styr_HB_D,endyr_HB_D,"set_log_F_dev_HB_D_vals");
  set_log_F_dev_GR_D_vals.allocate(styr_GR_D,endyr_GR_D,"set_log_F_dev_GR_D_vals");
  set_log_rec_dev_vals.allocate(styr_rec_dev,endyr_rec_dev,"set_log_rec_dev_vals");
  set_log_Nage_dev_vals.allocate(2,nages,"set_log_Nage_dev_vals");
  set_w_L.allocate("set_w_L");
  set_w_D.allocate("set_w_D");
  set_w_I_cH.allocate("set_w_I_cH");
  set_w_I_HB.allocate("set_w_I_HB");
  set_w_I_CVT.allocate("set_w_I_CVT");
  set_w_lc_cH.allocate("set_w_lc_cH");
  set_w_lc_cO.allocate("set_w_lc_cO");
  set_w_lc_HB.allocate("set_w_lc_HB");
  set_w_lc_GR.allocate("set_w_lc_GR");
  set_w_lc_HB_D.allocate("set_w_lc_HB_D");
  set_w_lc_CVT.allocate("set_w_lc_CVT");
  set_w_ac_cH.allocate("set_w_ac_cH");
  set_w_ac_HB.allocate("set_w_ac_HB");
  set_w_ac_GR.allocate("set_w_ac_GR");
  set_w_ac_CVT.allocate("set_w_ac_CVT");
  set_w_Nage_init.allocate("set_w_Nage_init");
  set_w_rec.allocate("set_w_rec");
  set_w_rec_early.allocate("set_w_rec_early");
  set_w_rec_end.allocate("set_w_rec_end");
  set_w_fullF.allocate("set_w_fullF");
  set_w_Ftune.allocate("set_w_Ftune");
  wgtpar_a.allocate("wgtpar_a");
  wgtpar_b.allocate("wgtpar_b");
  maturity_f_obs.allocate(1,nages,"maturity_f_obs");
  maturity_m_obs.allocate(1,nages,"maturity_m_obs");
  prop_f_obs.allocate(1,nages,"prop_f_obs");
  spawn_time_frac.allocate("spawn_time_frac");
  set_M.allocate(1,nages,"set_M");
  set_Dmort_cH.allocate("set_Dmort_cH");
  set_Dmort_HB.allocate("set_Dmort_HB");
  set_Dmort_GR.allocate("set_Dmort_GR");
  SR_switch.allocate("SR_switch");
  set_q_rate_phase.allocate("set_q_rate_phase");
  set_q_rate.allocate("set_q_rate");
  set_q_DD_phase.allocate("set_q_DD_phase");
  set_q_DD_beta.allocate("set_q_DD_beta");
  set_q_DD_beta_se.allocate("set_q_DD_beta_se");
  set_q_DD_stage.allocate("set_q_DD_stage");
  set_RWq_var.allocate("set_RWq_var");
  set_Ftune.allocate("set_Ftune");
  set_Ftune_yr.allocate("set_Ftune_yr");
  minSS_cH_lenc.allocate("minSS_cH_lenc");
  minSS_cO_lenc.allocate("minSS_cO_lenc");
  minSS_HB_lenc.allocate("minSS_HB_lenc");
  minSS_HB_D_lenc.allocate("minSS_HB_D_lenc");
  minSS_GR_lenc.allocate("minSS_GR_lenc");
  minSS_CVT_lenc.allocate("minSS_CVT_lenc");
  minSS_cH_agec.allocate("minSS_cH_agec");
  minSS_HB_agec.allocate("minSS_HB_agec");
  minSS_GR_agec.allocate("minSS_GR_agec");
  minSS_CVT_agec.allocate("minSS_CVT_agec");
  endyr_proj.allocate("endyr_proj");
  styr_regs.allocate("styr_regs");
  Fproj_switch.allocate("Fproj_switch");
  Fproj_mult.allocate("Fproj_mult");
   styr_proj=endyr+1;
  age_error.allocate(1,nages,1,nages,"age_error");
  end_of_data_file.allocate("end_of_data_file");
   if(end_of_data_file!=999)
   {
       cout << "*** WARNING: Data File NOT READ CORRECTLY ****" << endl;
       exit(0);  
   }
   else
   {cout << "Data File read correctly" << endl;} 
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  const double Linf_LO=set_Linf(2); const double Linf_HI=set_Linf(3); const double Linf_PH=set_Linf(4);
  const double K_LO=set_K(2); const double K_HI=set_K(3); const double K_PH=set_K(4);
  const double t0_LO=set_t0(2); const double t0_HI=set_t0(3); const double t0_PH=set_t0(4);  
  const double len_cv_LO=set_len_cv(2); const double len_cv_HI=set_len_cv(3); const double len_cv_PH=set_len_cv(4); 
     
  const double M_constant_LO=set_M_constant(2); const double M_constant_HI=set_M_constant(3); const double M_constant_PH=set_M_constant(4);        
  const double steep_LO=set_steep(2); const double steep_HI=set_steep(3); const double steep_PH=set_steep(4);
  const double log_R0_LO=set_log_R0(2); const double log_R0_HI=set_log_R0(3); const double log_R0_PH=set_log_R0(4);
  const double R_autocorr_LO=set_R_autocorr(2); const double R_autocorr_HI=set_R_autocorr(3); const double R_autocorr_PH=set_R_autocorr(4);
  const double rec_sigma_LO=set_rec_sigma(2); const double rec_sigma_HI=set_rec_sigma(3); const double rec_sigma_PH=set_rec_sigma(4);
  
  const double log_dm_cH_lc_LO=set_log_dm_cH_lc(2); const double log_dm_cH_lc_HI=set_log_dm_cH_lc(3); const double log_dm_cH_lc_PH=set_log_dm_cH_lc(4);
  const double log_dm_cO_lc_LO=set_log_dm_cO_lc(2); const double log_dm_cO_lc_HI=set_log_dm_cO_lc(3); const double log_dm_cO_lc_PH=set_log_dm_cO_lc(4);
  const double log_dm_HB_lc_LO=set_log_dm_HB_lc(2); const double log_dm_HB_lc_HI=set_log_dm_HB_lc(3); const double log_dm_HB_lc_PH=set_log_dm_HB_lc(4);
  const double log_dm_GR_lc_LO=set_log_dm_GR_lc(2); const double log_dm_GR_lc_HI=set_log_dm_GR_lc(3); const double log_dm_GR_lc_PH=set_log_dm_GR_lc(4);
  const double log_dm_HB_D_lc_LO=set_log_dm_HB_D_lc(2); const double log_dm_HB_D_lc_HI=set_log_dm_HB_D_lc(3); const double log_dm_HB_D_lc_PH=set_log_dm_HB_D_lc(4);
  const double log_dm_CVT_lc_LO=set_log_dm_CVT_lc(2); const double log_dm_CVT_lc_HI=set_log_dm_CVT_lc(3); const double log_dm_CVT_lc_PH=set_log_dm_CVT_lc(4);
  const double log_dm_cH_ac_LO=set_log_dm_cH_ac(2); const double log_dm_cH_ac_HI=set_log_dm_cH_ac(3); const double log_dm_cH_ac_PH=set_log_dm_cH_ac(4);
  const double log_dm_HB_ac_LO=set_log_dm_HB_ac(2); const double log_dm_HB_ac_HI=set_log_dm_HB_ac(3); const double log_dm_HB_ac_PH=set_log_dm_HB_ac(4);
  const double log_dm_GR_ac_LO=set_log_dm_GR_ac(2); const double log_dm_GR_ac_HI=set_log_dm_GR_ac(3); const double log_dm_GR_ac_PH=set_log_dm_GR_ac(4);
  const double log_dm_CVT_ac_LO=set_log_dm_CVT_ac(2); const double log_dm_CVT_ac_HI=set_log_dm_CVT_ac(3); const double log_dm_CVT_ac_PH=set_log_dm_CVT_ac(4);
  
  // const double selpar_A50_cH1_LO=set_selpar_A50_cH1(2); const double selpar_A50_cH1_HI=set_selpar_A50_cH1(3); const double selpar_A50_cH1_PH=set_selpar_A50_cH1(4);
  // const double selpar_slope_cH1_LO=set_selpar_slope_cH1(2); const double selpar_slope_cH1_HI=set_selpar_slope_cH1(3); const double selpar_slope_cH1_PH=set_selpar_slope_cH1(4);
  const double selpar_A50_cH2_LO=set_selpar_A50_cH2(2); const double selpar_A50_cH2_HI=set_selpar_A50_cH2(3); const double selpar_A50_cH2_PH=set_selpar_A50_cH2(4);
  const double selpar_slope_cH2_LO=set_selpar_slope_cH2(2); const double selpar_slope_cH2_HI=set_selpar_slope_cH2(3); const double selpar_slope_cH2_PH=set_selpar_slope_cH2(4);
  const double selpar_A50_cH3_LO=set_selpar_A50_cH3(2); const double selpar_A50_cH3_HI=set_selpar_A50_cH3(3); const double selpar_A50_cH3_PH=set_selpar_A50_cH3(4);
  const double selpar_slope_cH3_LO=set_selpar_slope_cH3(2); const double selpar_slope_cH3_HI=set_selpar_slope_cH3(3); const double selpar_slope_cH3_PH=set_selpar_slope_cH3(4);
  const double selpar_A50_cO2_LO=set_selpar_A50_cO2(2); const double selpar_A50_cO2_HI=set_selpar_A50_cO2(3); const double selpar_A50_cO2_PH=set_selpar_A50_cO2(4);
  const double selpar_A50_cO3_LO=set_selpar_A50_cO3(2); const double selpar_A50_cO3_HI=set_selpar_A50_cO3(3); const double selpar_A50_cO3_PH=set_selpar_A50_cO3(4);
  const double selpar_slope_cO2_LO=set_selpar_slope_cO2(2); const double selpar_slope_cO2_HI=set_selpar_slope_cO2(3); const double selpar_slope_cO2_PH=set_selpar_slope_cO2(4);
  const double selpar_A502_cO2_LO=set_selpar_A502_cO2(2); const double selpar_A502_cO2_HI=set_selpar_A502_cO2(3); const double selpar_A502_cO2_PH=set_selpar_A502_cO2(4);
  const double selpar_slope2_cO2_LO=set_selpar_slope2_cO2(2); const double selpar_slope2_cO2_HI=set_selpar_slope2_cO2(3); const double selpar_slope2_cO2_PH=set_selpar_slope2_cO2(4);
  
  const double selpar_A50_HB1_LO=set_selpar_A50_HB1(2); const double selpar_A50_HB1_HI=set_selpar_A50_HB1(3); const double selpar_A50_HB1_PH=set_selpar_A50_HB1(4);
  const double selpar_slope_HB1_LO=set_selpar_slope_HB1(2); const double selpar_slope_HB1_HI=set_selpar_slope_HB1(3); const double selpar_slope_HB1_PH=set_selpar_slope_HB1(4);
  const double selpar_A50_HB2_LO=set_selpar_A50_HB2(2); const double selpar_A50_HB2_HI=set_selpar_A50_HB2(3); const double selpar_A50_HB2_PH=set_selpar_A50_HB2(4);
  const double selpar_slope_HB2_LO=set_selpar_slope_HB2(2); const double selpar_slope_HB2_HI=set_selpar_slope_HB2(3); const double selpar_slope_HB2_PH=set_selpar_slope_HB2(4);
  const double selpar_A50_HB3_LO=set_selpar_A50_HB3(2); const double selpar_A50_HB3_HI=set_selpar_A50_HB3(3); const double selpar_A50_HB3_PH=set_selpar_A50_HB3(4);
  const double selpar_slope_HB3_LO=set_selpar_slope_HB3(2); const double selpar_slope_HB3_HI=set_selpar_slope_HB3(3); const double selpar_slope_HB3_PH=set_selpar_slope_HB3(4);
  
  const double selpar_A50_GR3_LO=set_selpar_A50_GR3(2); const double selpar_A50_GR3_HI=set_selpar_A50_GR3(3); const double selpar_A50_GR3_PH=set_selpar_A50_GR3(4);
  const double selpar_slope_GR3_LO=set_selpar_slope_GR3(2); const double selpar_slope_GR3_HI=set_selpar_slope_GR3(3); const double selpar_slope_GR3_PH=set_selpar_slope_GR3(4);
  
  const double selpar_A50_CVT_LO=set_selpar_A50_CVT(2); const double selpar_A50_CVT_HI=set_selpar_A50_CVT(3); const double selpar_A50_CVT_PH=set_selpar_A50_CVT(4);
  const double selpar_slope_CVT_LO=set_selpar_slope_CVT(2); const double selpar_slope_CVT_HI=set_selpar_slope_CVT(3); const double selpar_slope_CVT_PH=set_selpar_slope_CVT(4);
  const double selpar_A502_CVT_LO=set_selpar_A502_CVT(2); const double selpar_A502_CVT_HI=set_selpar_A502_CVT(3); const double selpar_A502_CVT_PH=set_selpar_A502_CVT(4);
  const double selpar_slope2_CVT_LO=set_selpar_slope2_CVT(2); const double selpar_slope2_CVT_HI=set_selpar_slope2_CVT(3); const double selpar_slope2_CVT_PH=set_selpar_slope2_CVT(4);
  
  const double selpar_age1logit_D_LO=set_selpar_age1logit_D(2); const double selpar_age1logit_D_HI=set_selpar_age1logit_D(3); const double selpar_age1logit_D_PH=set_selpar_age1logit_D(4);
  
  const double log_q_cH_LO=set_log_q_cH(2); const double log_q_cH_HI=set_log_q_cH(3); const double log_q_cH_PH=set_log_q_cH(4);
  const double log_q_HB_LO=set_log_q_HB(2); const double log_q_HB_HI=set_log_q_HB(3); const double log_q_HB_PH=set_log_q_HB(4);
  //const double log_q_GR_LO=set_log_q_GR(2); const double log_q_GR_HI=set_log_q_GR(3); const double log_q_GR_PH=set_log_q_GR(4);
  const double log_q_CVT_LO=set_log_q_CVT(2); const double log_q_CVT_HI=set_log_q_CVT(3); const double log_q_CVT_PH=set_log_q_CVT(4);
  
  const double log_avg_F_cH_LO=set_log_avg_F_cH(2); const double log_avg_F_cH_HI=set_log_avg_F_cH(3); const double log_avg_F_cH_PH=set_log_avg_F_cH(4);
  const double log_avg_F_cO_LO=set_log_avg_F_cO(2); const double log_avg_F_cO_HI=set_log_avg_F_cO(3); const double log_avg_F_cO_PH=set_log_avg_F_cO(4);
  const double log_avg_F_HB_LO=set_log_avg_F_HB(2); const double log_avg_F_HB_HI=set_log_avg_F_HB(3); const double log_avg_F_HB_PH=set_log_avg_F_HB(4); 
  const double log_avg_F_GR_LO=set_log_avg_F_GR(2); const double log_avg_F_GR_HI=set_log_avg_F_GR(3); const double log_avg_F_GR_PH=set_log_avg_F_GR(4); 
  const double log_avg_F_cH_D_LO=set_log_avg_F_cH_D(2); const double log_avg_F_cH_D_HI=set_log_avg_F_cH_D(3); const double log_avg_F_cH_D_PH=set_log_avg_F_cH_D(4);
  const double log_avg_F_HB_D_LO=set_log_avg_F_HB_D(2); const double log_avg_F_HB_D_HI=set_log_avg_F_HB_D(3); const double log_avg_F_HB_D_PH=set_log_avg_F_HB_D(4); 
  const double log_avg_F_GR_D_LO=set_log_avg_F_GR_D(2); const double log_avg_F_GR_D_HI=set_log_avg_F_GR_D(3); const double log_avg_F_GR_D_PH=set_log_avg_F_GR_D(4); 
  
  //-dev vectors-----------------------------------------------------------------------------------------------------------  
  const double log_F_dev_cH_LO=set_log_F_dev_cH(1); const double log_F_dev_cH_HI=set_log_F_dev_cH(2); const double log_F_dev_cH_PH=set_log_F_dev_cH(3);  
  const double log_F_dev_cO_LO=set_log_F_dev_cO(1); const double log_F_dev_cO_HI=set_log_F_dev_cO(2); const double log_F_dev_cO_PH=set_log_F_dev_cO(3);   
  const double log_F_dev_HB_LO=set_log_F_dev_HB(1); const double log_F_dev_HB_HI=set_log_F_dev_HB(2); const double log_F_dev_HB_PH=set_log_F_dev_HB(3);   
  const double log_F_dev_GR_LO=set_log_F_dev_GR(1); const double log_F_dev_GR_HI=set_log_F_dev_GR(2); const double log_F_dev_GR_PH=set_log_F_dev_GR(3);   
  
  const double log_F_dev_cH_D_LO=set_log_F_dev_cH_D(1); const double log_F_dev_cH_D_HI=set_log_F_dev_cH_D(2); const double log_F_dev_cH_D_PH=set_log_F_dev_cH_D(3);   
  const double log_F_dev_HB_D_LO=set_log_F_dev_HB_D(1); const double log_F_dev_HB_D_HI=set_log_F_dev_HB_D(2); const double log_F_dev_HB_D_PH=set_log_F_dev_HB_D(3);   
  const double log_F_dev_GR_D_LO=set_log_F_dev_GR_D(1); const double log_F_dev_GR_D_HI=set_log_F_dev_GR_D(2); const double log_F_dev_GR_D_PH=set_log_F_dev_GR_D(3);   
  
  const double log_RWq_LO=set_log_RWq_dev(1); const double log_RWq_HI=set_log_RWq_dev(2); const double log_RWq_PH=set_log_RWq_dev(3);  
  
  const double log_rec_dev_LO=set_log_rec_dev(1); const double log_rec_dev_HI=set_log_rec_dev(2); const double log_rec_dev_PH=set_log_rec_dev(3);          
  const double log_Nage_dev_LO=set_log_Nage_dev(1); const double log_Nage_dev_HI=set_log_Nage_dev(2); const double log_Nage_dev_PH=set_log_Nage_dev(3);          
  
  Linf.allocate(Linf_LO,Linf_HI,Linf_PH,"Linf");
  K.allocate(K_LO,K_HI,K_PH,"K");
  t0.allocate(t0_LO,t0_HI,t0_PH,"t0");
  len_cv_val.allocate(len_cv_LO,len_cv_HI,len_cv_PH,"len_cv_val");
  Linf_out.allocate(1,8,"Linf_out");
  #ifndef NO_AD_INITIALIZE
    Linf_out.initialize();
  #endif
  K_out.allocate(1,8,"K_out");
  #ifndef NO_AD_INITIALIZE
    K_out.initialize();
  #endif
  t0_out.allocate(1,8,"t0_out");
  #ifndef NO_AD_INITIALIZE
    t0_out.initialize();
  #endif
  len_cv_val_out.allocate(1,8,"len_cv_val_out");
  #ifndef NO_AD_INITIALIZE
    len_cv_val_out.initialize();
  #endif
  meanlen_TL.allocate(1,nages,"meanlen_TL");
  #ifndef NO_AD_INITIALIZE
    meanlen_TL.initialize();
  #endif
  wgt_g.allocate(1,nages,"wgt_g");
  #ifndef NO_AD_INITIALIZE
    wgt_g.initialize();
  #endif
  wgt_kg.allocate(1,nages,"wgt_kg");
  #ifndef NO_AD_INITIALIZE
    wgt_kg.initialize();
  #endif
  wgt_mt.allocate(1,nages,"wgt_mt");
  #ifndef NO_AD_INITIALIZE
    wgt_mt.initialize();
  #endif
  wgt_klb.allocate(1,nages,"wgt_klb");
  #ifndef NO_AD_INITIALIZE
    wgt_klb.initialize();
  #endif
  wgt_lb.allocate(1,nages,"wgt_lb");
  #ifndef NO_AD_INITIALIZE
    wgt_lb.initialize();
  #endif
  len_cH_mm.allocate(styr,endyr,1,nages,"len_cH_mm");
  #ifndef NO_AD_INITIALIZE
    len_cH_mm.initialize();
  #endif
  wholewgt_cH_klb.allocate(styr,endyr,1,nages,"wholewgt_cH_klb");
  #ifndef NO_AD_INITIALIZE
    wholewgt_cH_klb.initialize();
  #endif
  len_cO_mm.allocate(styr,endyr,1,nages,"len_cO_mm");
  #ifndef NO_AD_INITIALIZE
    len_cO_mm.initialize();
  #endif
  wholewgt_cO_klb.allocate(styr,endyr,1,nages,"wholewgt_cO_klb");
  #ifndef NO_AD_INITIALIZE
    wholewgt_cO_klb.initialize();
  #endif
  len_HB_mm.allocate(styr,endyr,1,nages,"len_HB_mm");
  #ifndef NO_AD_INITIALIZE
    len_HB_mm.initialize();
  #endif
  wholewgt_HB_klb.allocate(styr,endyr,1,nages,"wholewgt_HB_klb");
  #ifndef NO_AD_INITIALIZE
    wholewgt_HB_klb.initialize();
  #endif
  len_GR_mm.allocate(styr,endyr,1,nages,"len_GR_mm");
  #ifndef NO_AD_INITIALIZE
    len_GR_mm.initialize();
  #endif
  wholewgt_GR_klb.allocate(styr,endyr,1,nages,"wholewgt_GR_klb");
  #ifndef NO_AD_INITIALIZE
    wholewgt_GR_klb.initialize();
  #endif
  len_cH_D_mm.allocate(styr,endyr,1,nages,"len_cH_D_mm");
  #ifndef NO_AD_INITIALIZE
    len_cH_D_mm.initialize();
  #endif
  wholewgt_cH_D_klb.allocate(styr,endyr,1,nages,"wholewgt_cH_D_klb");
  #ifndef NO_AD_INITIALIZE
    wholewgt_cH_D_klb.initialize();
  #endif
  len_HB_D_mm.allocate(styr,endyr,1,nages,"len_HB_D_mm");
  #ifndef NO_AD_INITIALIZE
    len_HB_D_mm.initialize();
  #endif
  wholewgt_HB_D_klb.allocate(styr,endyr,1,nages,"wholewgt_HB_D_klb");
  #ifndef NO_AD_INITIALIZE
    wholewgt_HB_D_klb.initialize();
  #endif
  len_GR_D_mm.allocate(styr,endyr,1,nages,"len_GR_D_mm");
  #ifndef NO_AD_INITIALIZE
    len_GR_D_mm.initialize();
  #endif
  wholewgt_GR_D_klb.allocate(styr,endyr,1,nages,"wholewgt_GR_D_klb");
  #ifndef NO_AD_INITIALIZE
    wholewgt_GR_D_klb.initialize();
  #endif
  lenprob.allocate(1,nages,1,nlenbins,"lenprob");
  #ifndef NO_AD_INITIALIZE
    lenprob.initialize();
  #endif
  zscore_len.allocate("zscore_len");
  #ifndef NO_AD_INITIALIZE
  zscore_len.initialize();
  #endif
  cprob_lenvec.allocate(1,nlenbins,"cprob_lenvec");
  #ifndef NO_AD_INITIALIZE
    cprob_lenvec.initialize();
  #endif
  zscore_lzero.allocate("zscore_lzero");
  #ifndef NO_AD_INITIALIZE
  zscore_lzero.initialize();
  #endif
  cprob_lzero.allocate("cprob_lzero");
  #ifndef NO_AD_INITIALIZE
  cprob_lzero.initialize();
  #endif
  lenprob_cH.allocate(1,nages,1,nlenbins,"lenprob_cH");
  #ifndef NO_AD_INITIALIZE
    lenprob_cH.initialize();
  #endif
  lenprob_cO.allocate(1,nages,1,nlenbins,"lenprob_cO");
  #ifndef NO_AD_INITIALIZE
    lenprob_cO.initialize();
  #endif
  lenprob_HB.allocate(1,nages,1,nlenbins,"lenprob_HB");
  #ifndef NO_AD_INITIALIZE
    lenprob_HB.initialize();
  #endif
  lenprob_HB_D.allocate(1,nages,1,nlenbins,"lenprob_HB_D");
  #ifndef NO_AD_INITIALIZE
    lenprob_HB_D.initialize();
  #endif
  lenprob_GR.allocate(1,nages,1,nlenbins,"lenprob_GR");
  #ifndef NO_AD_INITIALIZE
    lenprob_GR.initialize();
  #endif
  lenprob_CVT.allocate(1,nages,1,nlenbins,"lenprob_CVT");
  #ifndef NO_AD_INITIALIZE
    lenprob_CVT.initialize();
  #endif
  len_sd.allocate(1,nages,"len_sd");
  #ifndef NO_AD_INITIALIZE
    len_sd.initialize();
  #endif
  len_cv.allocate(1,nages,"len_cv");
  #ifndef NO_AD_INITIALIZE
    len_cv.initialize();
  #endif
  pred_cH_lenc.allocate(1,nyr_cH_lenc,1,nlenbins,"pred_cH_lenc");
  #ifndef NO_AD_INITIALIZE
    pred_cH_lenc.initialize();
  #endif
  pred_cO_lenc.allocate(1,nyr_cO_lenc,1,nlenbins,"pred_cO_lenc");
  #ifndef NO_AD_INITIALIZE
    pred_cO_lenc.initialize();
  #endif
  pred_HB_lenc.allocate(1,nyr_HB_lenc,1,nlenbins,"pred_HB_lenc");
  #ifndef NO_AD_INITIALIZE
    pred_HB_lenc.initialize();
  #endif
  pred_HB_D_lenc.allocate(1,nyr_HB_D_lenc,1,nlenbins,"pred_HB_D_lenc");
  #ifndef NO_AD_INITIALIZE
    pred_HB_D_lenc.initialize();
  #endif
  pred_GR_lenc.allocate(1,nyr_GR_lenc,1,nlenbins,"pred_GR_lenc");
  #ifndef NO_AD_INITIALIZE
    pred_GR_lenc.initialize();
  #endif
  pred_CVT_lenc.allocate(1,nyr_CVT_lenc,1,nlenbins,"pred_CVT_lenc");
  #ifndef NO_AD_INITIALIZE
    pred_CVT_lenc.initialize();
  #endif
  pred_cH_agec.allocate(1,nyr_cH_agec,1,nages_agec,"pred_cH_agec");
  #ifndef NO_AD_INITIALIZE
    pred_cH_agec.initialize();
  #endif
  pred_cH_agec_allages.allocate(1,nyr_cH_agec,1,nages,"pred_cH_agec_allages");
  #ifndef NO_AD_INITIALIZE
    pred_cH_agec_allages.initialize();
  #endif
  ErrorFree_cH_agec.allocate(1,nyr_cH_agec,1,nages,"ErrorFree_cH_agec");
  #ifndef NO_AD_INITIALIZE
    ErrorFree_cH_agec.initialize();
  #endif
  pred_HB_agec.allocate(1,nyr_HB_agec,1,nages_agec,"pred_HB_agec");
  #ifndef NO_AD_INITIALIZE
    pred_HB_agec.initialize();
  #endif
  pred_HB_agec_allages.allocate(1,nyr_HB_agec,1,nages,"pred_HB_agec_allages");
  #ifndef NO_AD_INITIALIZE
    pred_HB_agec_allages.initialize();
  #endif
  ErrorFree_HB_agec.allocate(1,nyr_HB_agec,1,nages,"ErrorFree_HB_agec");
  #ifndef NO_AD_INITIALIZE
    ErrorFree_HB_agec.initialize();
  #endif
  pred_GR_agec.allocate(1,nyr_GR_agec,1,nages_agec,"pred_GR_agec");
  #ifndef NO_AD_INITIALIZE
    pred_GR_agec.initialize();
  #endif
  pred_GR_agec_allages.allocate(1,nyr_GR_agec,1,nages,"pred_GR_agec_allages");
  #ifndef NO_AD_INITIALIZE
    pred_GR_agec_allages.initialize();
  #endif
  ErrorFree_GR_agec.allocate(1,nyr_GR_agec,1,nages,"ErrorFree_GR_agec");
  #ifndef NO_AD_INITIALIZE
    ErrorFree_GR_agec.initialize();
  #endif
  pred_CVT_agec.allocate(1,nyr_CVT_agec,1,nages_agec,"pred_CVT_agec");
  #ifndef NO_AD_INITIALIZE
    pred_CVT_agec.initialize();
  #endif
  pred_CVT_agec_allages.allocate(1,nyr_CVT_agec,1,nages,"pred_CVT_agec_allages");
  #ifndef NO_AD_INITIALIZE
    pred_CVT_agec_allages.initialize();
  #endif
  ErrorFree_CVT_agec.allocate(1,nyr_CVT_agec,1,nages,"ErrorFree_CVT_agec");
  #ifndef NO_AD_INITIALIZE
    ErrorFree_CVT_agec.initialize();
  #endif
  nsamp_cH_lenc_allyr.allocate(styr,endyr,"nsamp_cH_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    nsamp_cH_lenc_allyr.initialize();
  #endif
  nsamp_cO_lenc_allyr.allocate(styr,endyr,"nsamp_cO_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    nsamp_cO_lenc_allyr.initialize();
  #endif
  nsamp_HB_lenc_allyr.allocate(styr,endyr,"nsamp_HB_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    nsamp_HB_lenc_allyr.initialize();
  #endif
  nsamp_HB_D_lenc_allyr.allocate(styr,endyr,"nsamp_HB_D_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    nsamp_HB_D_lenc_allyr.initialize();
  #endif
  nsamp_GR_lenc_allyr.allocate(styr,endyr,"nsamp_GR_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    nsamp_GR_lenc_allyr.initialize();
  #endif
  nsamp_CVT_lenc_allyr.allocate(styr,endyr,"nsamp_CVT_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    nsamp_CVT_lenc_allyr.initialize();
  #endif
  nsamp_cH_agec_allyr.allocate(styr,endyr,"nsamp_cH_agec_allyr");
  #ifndef NO_AD_INITIALIZE
    nsamp_cH_agec_allyr.initialize();
  #endif
  nsamp_HB_agec_allyr.allocate(styr,endyr,"nsamp_HB_agec_allyr");
  #ifndef NO_AD_INITIALIZE
    nsamp_HB_agec_allyr.initialize();
  #endif
  nsamp_GR_agec_allyr.allocate(styr,endyr,"nsamp_GR_agec_allyr");
  #ifndef NO_AD_INITIALIZE
    nsamp_GR_agec_allyr.initialize();
  #endif
  nsamp_CVT_agec_allyr.allocate(styr,endyr,"nsamp_CVT_agec_allyr");
  #ifndef NO_AD_INITIALIZE
    nsamp_CVT_agec_allyr.initialize();
  #endif
  nfish_cH_lenc_allyr.allocate(styr,endyr,"nfish_cH_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    nfish_cH_lenc_allyr.initialize();
  #endif
  nfish_cO_lenc_allyr.allocate(styr,endyr,"nfish_cO_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    nfish_cO_lenc_allyr.initialize();
  #endif
  nfish_HB_lenc_allyr.allocate(styr,endyr,"nfish_HB_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    nfish_HB_lenc_allyr.initialize();
  #endif
  nfish_HB_D_lenc_allyr.allocate(styr,endyr,"nfish_HB_D_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    nfish_HB_D_lenc_allyr.initialize();
  #endif
  nfish_GR_lenc_allyr.allocate(styr,endyr,"nfish_GR_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    nfish_GR_lenc_allyr.initialize();
  #endif
  nfish_CVT_lenc_allyr.allocate(styr,endyr,"nfish_CVT_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    nfish_CVT_lenc_allyr.initialize();
  #endif
  nfish_cH_agec_allyr.allocate(styr,endyr,"nfish_cH_agec_allyr");
  #ifndef NO_AD_INITIALIZE
    nfish_cH_agec_allyr.initialize();
  #endif
  nfish_HB_agec_allyr.allocate(styr,endyr,"nfish_HB_agec_allyr");
  #ifndef NO_AD_INITIALIZE
    nfish_HB_agec_allyr.initialize();
  #endif
  nfish_GR_agec_allyr.allocate(styr,endyr,"nfish_GR_agec_allyr");
  #ifndef NO_AD_INITIALIZE
    nfish_GR_agec_allyr.initialize();
  #endif
  nfish_CVT_agec_allyr.allocate(styr,endyr,"nfish_CVT_agec_allyr");
  #ifndef NO_AD_INITIALIZE
    nfish_CVT_agec_allyr.initialize();
  #endif
  neff_cH_lenc_allyr.allocate(styr,endyr,"neff_cH_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    neff_cH_lenc_allyr.initialize();
  #endif
  neff_cO_lenc_allyr.allocate(styr,endyr,"neff_cO_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    neff_cO_lenc_allyr.initialize();
  #endif
  neff_HB_lenc_allyr.allocate(styr,endyr,"neff_HB_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    neff_HB_lenc_allyr.initialize();
  #endif
  neff_HB_D_lenc_allyr.allocate(styr,endyr,"neff_HB_D_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    neff_HB_D_lenc_allyr.initialize();
  #endif
  neff_GR_lenc_allyr.allocate(styr,endyr,"neff_GR_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    neff_GR_lenc_allyr.initialize();
  #endif
  neff_CVT_lenc_allyr.allocate(styr,endyr,"neff_CVT_lenc_allyr");
  #ifndef NO_AD_INITIALIZE
    neff_CVT_lenc_allyr.initialize();
  #endif
  neff_cH_agec_allyr.allocate(styr,endyr,"neff_cH_agec_allyr");
  #ifndef NO_AD_INITIALIZE
    neff_cH_agec_allyr.initialize();
  #endif
  neff_HB_agec_allyr.allocate(styr,endyr,"neff_HB_agec_allyr");
  #ifndef NO_AD_INITIALIZE
    neff_HB_agec_allyr.initialize();
  #endif
  neff_GR_agec_allyr.allocate(styr,endyr,"neff_GR_agec_allyr");
  #ifndef NO_AD_INITIALIZE
    neff_GR_agec_allyr.initialize();
  #endif
  neff_CVT_agec_allyr.allocate(styr,endyr,"neff_CVT_agec_allyr");
  #ifndef NO_AD_INITIALIZE
    neff_CVT_agec_allyr.initialize();
  #endif
  N.allocate(styr,endyr+1,1,nages,"N");
  #ifndef NO_AD_INITIALIZE
    N.initialize();
  #endif
  N_mdyr.allocate(styr,endyr,1,nages,"N_mdyr");
  #ifndef NO_AD_INITIALIZE
    N_mdyr.initialize();
  #endif
  N_spawn.allocate(styr,endyr,1,nages,"N_spawn");
  #ifndef NO_AD_INITIALIZE
    N_spawn.initialize();
  #endif
  log_Nage_dev.allocate(2,nages,log_Nage_dev_LO,log_Nage_dev_HI,log_Nage_dev_PH,"log_Nage_dev");
  log_Nage_dev_output.allocate(1,nages,"log_Nage_dev_output");
  #ifndef NO_AD_INITIALIZE
    log_Nage_dev_output.initialize();
  #endif
  B.allocate(styr,endyr+1,1,nages,"B");
  #ifndef NO_AD_INITIALIZE
    B.initialize();
  #endif
  totB.allocate(styr,endyr+1,"totB");
  #ifndef NO_AD_INITIALIZE
    totB.initialize();
  #endif
  totN.allocate(styr,endyr+1,"totN");
  #ifndef NO_AD_INITIALIZE
    totN.initialize();
  #endif
  SSB.allocate(styr,endyr,"SSB");
  #ifndef NO_AD_INITIALIZE
    SSB.initialize();
  #endif
  rec.allocate(styr,endyr+1,"rec");
  #ifndef NO_AD_INITIALIZE
    rec.initialize();
  #endif
  prop_f.allocate(1,nages,"prop_f");
  #ifndef NO_AD_INITIALIZE
    prop_f.initialize();
  #endif
  prop_m.allocate(1,nages,"prop_m");
  #ifndef NO_AD_INITIALIZE
    prop_m.initialize();
  #endif
  maturity_f.allocate(1,nages,"maturity_f");
  #ifndef NO_AD_INITIALIZE
    maturity_f.initialize();
  #endif
  maturity_m.allocate(1,nages,"maturity_m");
  #ifndef NO_AD_INITIALIZE
    maturity_m.initialize();
  #endif
  reprod.allocate(1,nages,"reprod");
  #ifndef NO_AD_INITIALIZE
    reprod.initialize();
  #endif
  log_R0.allocate(log_R0_LO,log_R0_HI,log_R0_PH,"log_R0");
  log_R0_out.allocate(1,8,"log_R0_out");
  #ifndef NO_AD_INITIALIZE
    log_R0_out.initialize();
  #endif
  R0.allocate("R0");
  #ifndef NO_AD_INITIALIZE
  R0.initialize();
  #endif
  steep.allocate(steep_LO,steep_HI,steep_PH,"steep");
  steep_out.allocate(1,8,"steep_out");
  #ifndef NO_AD_INITIALIZE
    steep_out.initialize();
  #endif
  rec_sigma.allocate(rec_sigma_LO,rec_sigma_HI,rec_sigma_PH,"rec_sigma");
  rec_sigma_out.allocate(1,8,"rec_sigma_out");
  #ifndef NO_AD_INITIALIZE
    rec_sigma_out.initialize();
  #endif
  R_autocorr.allocate(R_autocorr_LO,R_autocorr_HI,R_autocorr_PH,"R_autocorr");
  R_autocorr_out.allocate(1,8,"R_autocorr_out");
  #ifndef NO_AD_INITIALIZE
    R_autocorr_out.initialize();
  #endif
  rec_sigma_sq.allocate("rec_sigma_sq");
  #ifndef NO_AD_INITIALIZE
  rec_sigma_sq.initialize();
  #endif
  rec_logL_add.allocate("rec_logL_add");
  #ifndef NO_AD_INITIALIZE
  rec_logL_add.initialize();
  #endif
  log_rec_dev.allocate(styr_rec_dev,endyr_rec_dev,log_rec_dev_LO,log_rec_dev_HI,log_rec_dev_PH,"log_rec_dev");
  log_rec_dev_output.allocate(styr,endyr+1,"log_rec_dev_output");
  #ifndef NO_AD_INITIALIZE
    log_rec_dev_output.initialize();
  #endif
  log_rec_dev_out.allocate(styr_rec_dev,endyr_rec_dev,"log_rec_dev_out");
  #ifndef NO_AD_INITIALIZE
    log_rec_dev_out.initialize();
  #endif
  var_rec_dev.allocate("var_rec_dev");
  #ifndef NO_AD_INITIALIZE
  var_rec_dev.initialize();
  #endif
  sigma_rec_dev.allocate("sigma_rec_dev");
  #ifndef NO_AD_INITIALIZE
  sigma_rec_dev.initialize();
  #endif
  BiasCor.allocate("BiasCor");
  #ifndef NO_AD_INITIALIZE
  BiasCor.initialize();
  #endif
  S0.allocate("S0");
  #ifndef NO_AD_INITIALIZE
  S0.initialize();
  #endif
  B0.allocate("B0");
  #ifndef NO_AD_INITIALIZE
  B0.initialize();
  #endif
  R1.allocate("R1");
  #ifndef NO_AD_INITIALIZE
  R1.initialize();
  #endif
  R_virgin.allocate("R_virgin");
  #ifndef NO_AD_INITIALIZE
  R_virgin.initialize();
  #endif
  SdS0.allocate(styr,endyr,"SdS0");
  #ifndef NO_AD_INITIALIZE
    SdS0.initialize();
  #endif
  log_dm_cH_lc.allocate(log_dm_cH_lc_LO,log_dm_cH_lc_HI,log_dm_cH_lc_PH,"log_dm_cH_lc");
  log_dm_cO_lc.allocate(log_dm_cO_lc_LO,log_dm_cO_lc_HI,log_dm_cO_lc_PH,"log_dm_cO_lc");
  log_dm_HB_lc.allocate(log_dm_HB_lc_LO,log_dm_HB_lc_HI,log_dm_HB_lc_PH,"log_dm_HB_lc");
  log_dm_GR_lc.allocate(log_dm_GR_lc_LO,log_dm_GR_lc_HI,log_dm_GR_lc_PH,"log_dm_GR_lc");
  log_dm_HB_D_lc.allocate(log_dm_HB_D_lc_LO,log_dm_HB_D_lc_HI,log_dm_HB_D_lc_PH,"log_dm_HB_D_lc");
  log_dm_CVT_lc.allocate(log_dm_CVT_lc_LO,log_dm_CVT_lc_HI,log_dm_CVT_lc_PH,"log_dm_CVT_lc");
  log_dm_cH_ac.allocate(log_dm_cH_ac_LO,log_dm_cH_ac_HI,log_dm_cH_ac_PH,"log_dm_cH_ac");
  log_dm_HB_ac.allocate(log_dm_HB_ac_LO,log_dm_HB_ac_HI,log_dm_HB_ac_PH,"log_dm_HB_ac");
  log_dm_GR_ac.allocate(log_dm_GR_ac_LO,log_dm_GR_ac_HI,log_dm_GR_ac_PH,"log_dm_GR_ac");
  log_dm_CVT_ac.allocate(log_dm_CVT_ac_LO,log_dm_CVT_ac_HI,log_dm_CVT_ac_PH,"log_dm_CVT_ac");
  log_dm_cH_lc_out.allocate(1,8,"log_dm_cH_lc_out");
  #ifndef NO_AD_INITIALIZE
    log_dm_cH_lc_out.initialize();
  #endif
  log_dm_cO_lc_out.allocate(1,8,"log_dm_cO_lc_out");
  #ifndef NO_AD_INITIALIZE
    log_dm_cO_lc_out.initialize();
  #endif
  log_dm_HB_lc_out.allocate(1,8,"log_dm_HB_lc_out");
  #ifndef NO_AD_INITIALIZE
    log_dm_HB_lc_out.initialize();
  #endif
  log_dm_GR_lc_out.allocate(1,8,"log_dm_GR_lc_out");
  #ifndef NO_AD_INITIALIZE
    log_dm_GR_lc_out.initialize();
  #endif
  log_dm_HB_D_lc_out.allocate(1,8,"log_dm_HB_D_lc_out");
  #ifndef NO_AD_INITIALIZE
    log_dm_HB_D_lc_out.initialize();
  #endif
  log_dm_CVT_lc_out.allocate(1,8,"log_dm_CVT_lc_out");
  #ifndef NO_AD_INITIALIZE
    log_dm_CVT_lc_out.initialize();
  #endif
  log_dm_cH_ac_out.allocate(1,8,"log_dm_cH_ac_out");
  #ifndef NO_AD_INITIALIZE
    log_dm_cH_ac_out.initialize();
  #endif
  log_dm_HB_ac_out.allocate(1,8,"log_dm_HB_ac_out");
  #ifndef NO_AD_INITIALIZE
    log_dm_HB_ac_out.initialize();
  #endif
  log_dm_GR_ac_out.allocate(1,8,"log_dm_GR_ac_out");
  #ifndef NO_AD_INITIALIZE
    log_dm_GR_ac_out.initialize();
  #endif
  log_dm_CVT_ac_out.allocate(1,8,"log_dm_CVT_ac_out");
  #ifndef NO_AD_INITIALIZE
    log_dm_CVT_ac_out.initialize();
  #endif
  sel_cH.allocate(styr,endyr,1,nages,"sel_cH");
  #ifndef NO_AD_INITIALIZE
    sel_cH.initialize();
  #endif
  sel_cH_block1.allocate(1,nages,"sel_cH_block1");
  #ifndef NO_AD_INITIALIZE
    sel_cH_block1.initialize();
  #endif
  sel_cH_block2.allocate(1,nages,"sel_cH_block2");
  #ifndef NO_AD_INITIALIZE
    sel_cH_block2.initialize();
  #endif
  sel_cH_block3.allocate(1,nages,"sel_cH_block3");
  #ifndef NO_AD_INITIALIZE
    sel_cH_block3.initialize();
  #endif
  selpar_A50_cH2.allocate(selpar_A50_cH2_LO,selpar_A50_cH2_HI,selpar_A50_cH2_PH,"selpar_A50_cH2");
  selpar_slope_cH2.allocate(selpar_slope_cH2_LO,selpar_slope_cH2_HI,selpar_slope_cH2_PH,"selpar_slope_cH2");
  selpar_A50_cH3.allocate(selpar_A50_cH3_LO,selpar_A50_cH3_HI,selpar_A50_cH3_PH,"selpar_A50_cH3");
  selpar_slope_cH3.allocate(selpar_slope_cH3_LO,selpar_slope_cH3_HI,selpar_slope_cH3_PH,"selpar_slope_cH3");
  selpar_A50_cH2_out.allocate(1,8,"selpar_A50_cH2_out");
  #ifndef NO_AD_INITIALIZE
    selpar_A50_cH2_out.initialize();
  #endif
  selpar_slope_cH2_out.allocate(1,8,"selpar_slope_cH2_out");
  #ifndef NO_AD_INITIALIZE
    selpar_slope_cH2_out.initialize();
  #endif
  selpar_A50_cH3_out.allocate(1,8,"selpar_A50_cH3_out");
  #ifndef NO_AD_INITIALIZE
    selpar_A50_cH3_out.initialize();
  #endif
  selpar_slope_cH3_out.allocate(1,8,"selpar_slope_cH3_out");
  #ifndef NO_AD_INITIALIZE
    selpar_slope_cH3_out.initialize();
  #endif
  sel_cO.allocate(styr,endyr,1,nages,"sel_cO");
  #ifndef NO_AD_INITIALIZE
    sel_cO.initialize();
  #endif
  sel_cO_block1.allocate(1,nages,"sel_cO_block1");
  #ifndef NO_AD_INITIALIZE
    sel_cO_block1.initialize();
  #endif
  sel_cO_block2.allocate(1,nages,"sel_cO_block2");
  #ifndef NO_AD_INITIALIZE
    sel_cO_block2.initialize();
  #endif
  sel_cO_block3.allocate(1,nages,"sel_cO_block3");
  #ifndef NO_AD_INITIALIZE
    sel_cO_block3.initialize();
  #endif
  selpar_A50_cO2.allocate(selpar_A50_cO2_LO,selpar_A50_cO2_HI,selpar_A50_cO2_PH,"selpar_A50_cO2");
  selpar_A50_cO3.allocate(selpar_A50_cO3_LO,selpar_A50_cO3_HI,selpar_A50_cO3_PH,"selpar_A50_cO3");
  selpar_slope_cO2.allocate(selpar_slope_cO2_LO,selpar_slope_cO2_HI,selpar_slope_cO2_PH,"selpar_slope_cO2");
  selpar_A502_cO2.allocate(selpar_A502_cO2_LO,selpar_A502_cO2_HI,selpar_A502_cO2_PH,"selpar_A502_cO2");
  selpar_slope2_cO2.allocate(selpar_slope2_cO2_LO,selpar_slope2_cO2_HI,selpar_slope2_cO2_PH,"selpar_slope2_cO2");
  selpar_A50_cO2_out.allocate(1,8,"selpar_A50_cO2_out");
  #ifndef NO_AD_INITIALIZE
    selpar_A50_cO2_out.initialize();
  #endif
  selpar_A50_cO3_out.allocate(1,8,"selpar_A50_cO3_out");
  #ifndef NO_AD_INITIALIZE
    selpar_A50_cO3_out.initialize();
  #endif
  selpar_slope_cO2_out.allocate(1,8,"selpar_slope_cO2_out");
  #ifndef NO_AD_INITIALIZE
    selpar_slope_cO2_out.initialize();
  #endif
  selpar_A502_cO2_out.allocate(1,8,"selpar_A502_cO2_out");
  #ifndef NO_AD_INITIALIZE
    selpar_A502_cO2_out.initialize();
  #endif
  selpar_slope2_cO2_out.allocate(1,8,"selpar_slope2_cO2_out");
  #ifndef NO_AD_INITIALIZE
    selpar_slope2_cO2_out.initialize();
  #endif
  sel_HB.allocate(styr,endyr,1,nages,"sel_HB");
  #ifndef NO_AD_INITIALIZE
    sel_HB.initialize();
  #endif
  sel_HB_block1.allocate(1,nages,"sel_HB_block1");
  #ifndef NO_AD_INITIALIZE
    sel_HB_block1.initialize();
  #endif
  sel_HB_block2.allocate(1,nages,"sel_HB_block2");
  #ifndef NO_AD_INITIALIZE
    sel_HB_block2.initialize();
  #endif
  sel_HB_block3.allocate(1,nages,"sel_HB_block3");
  #ifndef NO_AD_INITIALIZE
    sel_HB_block3.initialize();
  #endif
  selpar_A50_HB1.allocate(selpar_A50_HB1_LO,selpar_A50_HB1_HI,selpar_A50_HB1_PH,"selpar_A50_HB1");
  selpar_slope_HB1.allocate(selpar_slope_HB1_LO,selpar_slope_HB1_HI,selpar_slope_HB1_PH,"selpar_slope_HB1");
  selpar_A50_HB2.allocate(selpar_A50_HB2_LO,selpar_A50_HB2_HI,selpar_A50_HB2_PH,"selpar_A50_HB2");
  selpar_slope_HB2.allocate(selpar_slope_HB2_LO,selpar_slope_HB2_HI,selpar_slope_HB2_PH,"selpar_slope_HB2");
  selpar_A50_HB3.allocate(selpar_A50_HB3_LO,selpar_A50_HB3_HI,selpar_A50_HB3_PH,"selpar_A50_HB3");
  selpar_slope_HB3.allocate(selpar_slope_HB3_LO,selpar_slope_HB3_HI,selpar_slope_HB3_PH,"selpar_slope_HB3");
  selpar_A50_HB1_out.allocate(1,8,"selpar_A50_HB1_out");
  #ifndef NO_AD_INITIALIZE
    selpar_A50_HB1_out.initialize();
  #endif
  selpar_slope_HB1_out.allocate(1,8,"selpar_slope_HB1_out");
  #ifndef NO_AD_INITIALIZE
    selpar_slope_HB1_out.initialize();
  #endif
  selpar_A50_HB2_out.allocate(1,8,"selpar_A50_HB2_out");
  #ifndef NO_AD_INITIALIZE
    selpar_A50_HB2_out.initialize();
  #endif
  selpar_slope_HB2_out.allocate(1,8,"selpar_slope_HB2_out");
  #ifndef NO_AD_INITIALIZE
    selpar_slope_HB2_out.initialize();
  #endif
  selpar_A50_HB3_out.allocate(1,8,"selpar_A50_HB3_out");
  #ifndef NO_AD_INITIALIZE
    selpar_A50_HB3_out.initialize();
  #endif
  selpar_slope_HB3_out.allocate(1,8,"selpar_slope_HB3_out");
  #ifndef NO_AD_INITIALIZE
    selpar_slope_HB3_out.initialize();
  #endif
  sel_GR.allocate(styr,endyr,1,nages,"sel_GR");
  #ifndef NO_AD_INITIALIZE
    sel_GR.initialize();
  #endif
  sel_GR_block1.allocate(1,nages,"sel_GR_block1");
  #ifndef NO_AD_INITIALIZE
    sel_GR_block1.initialize();
  #endif
  sel_GR_block2.allocate(1,nages,"sel_GR_block2");
  #ifndef NO_AD_INITIALIZE
    sel_GR_block2.initialize();
  #endif
  sel_GR_block3.allocate(1,nages,"sel_GR_block3");
  #ifndef NO_AD_INITIALIZE
    sel_GR_block3.initialize();
  #endif
  selpar_A50_GR3.allocate(selpar_A50_GR3_LO,selpar_A50_GR3_HI,selpar_A50_GR3_PH,"selpar_A50_GR3");
  selpar_slope_GR3.allocate(selpar_slope_GR3_LO,selpar_slope_GR3_HI,selpar_slope_GR3_PH,"selpar_slope_GR3");
  selpar_A50_GR3_out.allocate(1,8,"selpar_A50_GR3_out");
  #ifndef NO_AD_INITIALIZE
    selpar_A50_GR3_out.initialize();
  #endif
  selpar_slope_GR3_out.allocate(1,8,"selpar_slope_GR3_out");
  #ifndef NO_AD_INITIALIZE
    selpar_slope_GR3_out.initialize();
  #endif
  sel_D.allocate(styr,endyr,1,nages,"sel_D");
  #ifndef NO_AD_INITIALIZE
    sel_D.initialize();
  #endif
  sel_D_block1.allocate(1,nages,"sel_D_block1");
  #ifndef NO_AD_INITIALIZE
    sel_D_block1.initialize();
  #endif
  sel_D_block2.allocate(1,nages,"sel_D_block2");
  #ifndef NO_AD_INITIALIZE
    sel_D_block2.initialize();
  #endif
  sel_D_block3.allocate(1,nages,"sel_D_block3");
  #ifndef NO_AD_INITIALIZE
    sel_D_block3.initialize();
  #endif
  prob_belowsizelim_block2.allocate(1,nages,"prob_belowsizelim_block2");
  #ifndef NO_AD_INITIALIZE
    prob_belowsizelim_block2.initialize();
  #endif
  prob_belowsizelim_block3.allocate(1,nages,"prob_belowsizelim_block3");
  #ifndef NO_AD_INITIALIZE
    prob_belowsizelim_block3.initialize();
  #endif
  zscore_lsizelim1.allocate("zscore_lsizelim1");
  #ifndef NO_AD_INITIALIZE
  zscore_lsizelim1.initialize();
  #endif
  zscore_lsizelim2.allocate("zscore_lsizelim2");
  #ifndef NO_AD_INITIALIZE
  zscore_lsizelim2.initialize();
  #endif
  cprob_lsizelim1.allocate("cprob_lsizelim1");
  #ifndef NO_AD_INITIALIZE
  cprob_lsizelim1.initialize();
  #endif
  cprob_lsizelim2.allocate("cprob_lsizelim2");
  #ifndef NO_AD_INITIALIZE
  cprob_lsizelim2.initialize();
  #endif
  selpar_age1logit_D.allocate(selpar_age1logit_D_LO,selpar_age1logit_D_HI,selpar_age1logit_D_PH,"selpar_age1logit_D");
  selpar_age1_D.allocate("selpar_age1_D");
  #ifndef NO_AD_INITIALIZE
  selpar_age1_D.initialize();
  #endif
  selpar_age1logit_D_out.allocate(1,8,"selpar_age1logit_D_out");
  #ifndef NO_AD_INITIALIZE
    selpar_age1logit_D_out.initialize();
  #endif
  sel_CVT.allocate(styr,endyr,1,nages,"sel_CVT");
  #ifndef NO_AD_INITIALIZE
    sel_CVT.initialize();
  #endif
  sel_CVT_vec.allocate(1,nages,"sel_CVT_vec");
  #ifndef NO_AD_INITIALIZE
    sel_CVT_vec.initialize();
  #endif
  selpar_A50_CVT.allocate(selpar_A50_CVT_LO,selpar_A50_CVT_HI,selpar_A50_CVT_PH,"selpar_A50_CVT");
  selpar_slope_CVT.allocate(selpar_slope_CVT_LO,selpar_slope_CVT_HI,selpar_slope_CVT_PH,"selpar_slope_CVT");
  selpar_A502_CVT.allocate(selpar_A502_CVT_LO,selpar_A502_CVT_HI,selpar_A502_CVT_PH,"selpar_A502_CVT");
  selpar_slope2_CVT.allocate(selpar_slope2_CVT_LO,selpar_slope2_CVT_HI,selpar_slope2_CVT_PH,"selpar_slope2_CVT");
  selpar_A50_CVT_out.allocate(1,8,"selpar_A50_CVT_out");
  #ifndef NO_AD_INITIALIZE
    selpar_A50_CVT_out.initialize();
  #endif
  selpar_slope_CVT_out.allocate(1,8,"selpar_slope_CVT_out");
  #ifndef NO_AD_INITIALIZE
    selpar_slope_CVT_out.initialize();
  #endif
  selpar_A502_CVT_out.allocate(1,8,"selpar_A502_CVT_out");
  #ifndef NO_AD_INITIALIZE
    selpar_A502_CVT_out.initialize();
  #endif
  selpar_slope2_CVT_out.allocate(1,8,"selpar_slope2_CVT_out");
  #ifndef NO_AD_INITIALIZE
    selpar_slope2_CVT_out.initialize();
  #endif
  sel_wgted_L.allocate(1,nages,"sel_wgted_L");
  #ifndef NO_AD_INITIALIZE
    sel_wgted_L.initialize();
  #endif
  sel_wgted_D.allocate(1,nages,"sel_wgted_D");
  #ifndef NO_AD_INITIALIZE
    sel_wgted_D.initialize();
  #endif
  sel_wgted_tot.allocate(1,nages,"sel_wgted_tot");
  #ifndef NO_AD_INITIALIZE
    sel_wgted_tot.initialize();
  #endif
  pred_cH_cpue.allocate(styr_cH_cpue,endyr_cH_cpue,"pred_cH_cpue");
  #ifndef NO_AD_INITIALIZE
    pred_cH_cpue.initialize();
  #endif
  N_cH.allocate(styr_cH_cpue,endyr_cH_cpue,1,nages,"N_cH");
  #ifndef NO_AD_INITIALIZE
    N_cH.initialize();
  #endif
  pred_HB_cpue.allocate(styr_HB_cpue,endyr_HB_cpue,"pred_HB_cpue");
  #ifndef NO_AD_INITIALIZE
    pred_HB_cpue.initialize();
  #endif
  N_HB.allocate(styr_HB_cpue,endyr_HB_cpue,1,nages,"N_HB");
  #ifndef NO_AD_INITIALIZE
    N_HB.initialize();
  #endif
  pred_CVT_cpue.allocate(styr_CVT_cpue,endyr_CVT_cpue,"pred_CVT_cpue");
  #ifndef NO_AD_INITIALIZE
    pred_CVT_cpue.initialize();
  #endif
  N_CVT.allocate(styr_CVT_cpue,endyr_CVT_cpue,1,nages,"N_CVT");
  #ifndef NO_AD_INITIALIZE
    N_CVT.initialize();
  #endif
  log_q_cH.allocate(log_q_cH_LO,log_q_cH_HI,log_q_cH_PH,"log_q_cH");
  log_q_HB.allocate(log_q_HB_LO,log_q_HB_HI,log_q_HB_PH,"log_q_HB");
  log_q_CVT.allocate(log_q_CVT_LO,log_q_CVT_HI,log_q_CVT_PH,"log_q_CVT");
  log_q_cH_out.allocate(1,8,"log_q_cH_out");
  #ifndef NO_AD_INITIALIZE
    log_q_cH_out.initialize();
  #endif
  log_q_HB_out.allocate(1,8,"log_q_HB_out");
  #ifndef NO_AD_INITIALIZE
    log_q_HB_out.initialize();
  #endif
  log_q_CVT_out.allocate(1,8,"log_q_CVT_out");
  #ifndef NO_AD_INITIALIZE
    log_q_CVT_out.initialize();
  #endif
  q_rate.allocate("q_rate");
  #ifndef NO_AD_INITIALIZE
  q_rate.initialize();
  #endif
  q_rate_fcn_cH.allocate(styr_cH_cpue,endyr_cH_cpue,"q_rate_fcn_cH");
  #ifndef NO_AD_INITIALIZE
    q_rate_fcn_cH.initialize();
  #endif
  q_rate_fcn_HB.allocate(styr_HB_cpue,endyr_HB_cpue,"q_rate_fcn_HB");
  #ifndef NO_AD_INITIALIZE
    q_rate_fcn_HB.initialize();
  #endif
  q_DD_beta.allocate("q_DD_beta");
  #ifndef NO_AD_INITIALIZE
  q_DD_beta.initialize();
  #endif
  q_DD_fcn.allocate(styr,endyr,"q_DD_fcn");
  #ifndef NO_AD_INITIALIZE
    q_DD_fcn.initialize();
  #endif
  B0_q_DD.allocate("B0_q_DD");
  #ifndef NO_AD_INITIALIZE
  B0_q_DD.initialize();
  #endif
  B_q_DD.allocate(styr,endyr,"B_q_DD");
  #ifndef NO_AD_INITIALIZE
    B_q_DD.initialize();
  #endif
  q_RW_log_dev_cH.allocate(styr_cH_cpue,endyr_cH_cpue-1,log_RWq_LO,log_RWq_HI,log_RWq_PH,"q_RW_log_dev_cH");
  q_RW_log_dev_HB.allocate(styr_HB_cpue,endyr_HB_cpue-1,log_RWq_LO,log_RWq_HI,log_RWq_PH,"q_RW_log_dev_HB");
  q_RW_log_dev_CVT.allocate(styr_CVT_cpue,endyr_CVT_cpue-1,"q_RW_log_dev_CVT");
  #ifndef NO_AD_INITIALIZE
    q_RW_log_dev_CVT.initialize();
  #endif
  q_cH.allocate(styr_cH_cpue,endyr_cH_cpue,"q_cH");
  #ifndef NO_AD_INITIALIZE
    q_cH.initialize();
  #endif
  q_HB.allocate(styr_HB_cpue,endyr_HB_cpue,"q_HB");
  #ifndef NO_AD_INITIALIZE
    q_HB.initialize();
  #endif
  q_CVT.allocate(styr_CVT_cpue,endyr_CVT_cpue,"q_CVT");
  #ifndef NO_AD_INITIALIZE
    q_CVT.initialize();
  #endif
  L_cH_num.allocate(styr,endyr,1,nages,"L_cH_num");
  #ifndef NO_AD_INITIALIZE
    L_cH_num.initialize();
  #endif
  L_cH_klb.allocate(styr,endyr,1,nages,"L_cH_klb");
  #ifndef NO_AD_INITIALIZE
    L_cH_klb.initialize();
  #endif
  pred_cH_L_knum.allocate(styr,endyr,"pred_cH_L_knum");
  #ifndef NO_AD_INITIALIZE
    pred_cH_L_knum.initialize();
  #endif
  pred_cH_L_klb.allocate(styr,endyr,"pred_cH_L_klb");
  #ifndef NO_AD_INITIALIZE
    pred_cH_L_klb.initialize();
  #endif
  L_cO_num.allocate(styr,endyr,1,nages,"L_cO_num");
  #ifndef NO_AD_INITIALIZE
    L_cO_num.initialize();
  #endif
  L_cO_klb.allocate(styr,endyr,1,nages,"L_cO_klb");
  #ifndef NO_AD_INITIALIZE
    L_cO_klb.initialize();
  #endif
  pred_cO_L_knum.allocate(styr,endyr,"pred_cO_L_knum");
  #ifndef NO_AD_INITIALIZE
    pred_cO_L_knum.initialize();
  #endif
  pred_cO_L_klb.allocate(styr,endyr,"pred_cO_L_klb");
  #ifndef NO_AD_INITIALIZE
    pred_cO_L_klb.initialize();
  #endif
  L_HB_num.allocate(styr,endyr,1,nages,"L_HB_num");
  #ifndef NO_AD_INITIALIZE
    L_HB_num.initialize();
  #endif
  L_HB_klb.allocate(styr,endyr,1,nages,"L_HB_klb");
  #ifndef NO_AD_INITIALIZE
    L_HB_klb.initialize();
  #endif
  pred_HB_L_knum.allocate(styr,endyr,"pred_HB_L_knum");
  #ifndef NO_AD_INITIALIZE
    pred_HB_L_knum.initialize();
  #endif
  pred_HB_L_klb.allocate(styr,endyr,"pred_HB_L_klb");
  #ifndef NO_AD_INITIALIZE
    pred_HB_L_klb.initialize();
  #endif
  L_GR_num.allocate(styr,endyr,1,nages,"L_GR_num");
  #ifndef NO_AD_INITIALIZE
    L_GR_num.initialize();
  #endif
  L_GR_klb.allocate(styr,endyr,1,nages,"L_GR_klb");
  #ifndef NO_AD_INITIALIZE
    L_GR_klb.initialize();
  #endif
  pred_GR_L_knum.allocate(styr,endyr,"pred_GR_L_knum");
  #ifndef NO_AD_INITIALIZE
    pred_GR_L_knum.initialize();
  #endif
  pred_GR_L_klb.allocate(styr,endyr,"pred_GR_L_klb");
  #ifndef NO_AD_INITIALIZE
    pred_GR_L_klb.initialize();
  #endif
  L_total_num.allocate(styr,endyr,1,nages,"L_total_num");
  #ifndef NO_AD_INITIALIZE
    L_total_num.initialize();
  #endif
  L_total_klb.allocate(styr,endyr,1,nages,"L_total_klb");
  #ifndef NO_AD_INITIALIZE
    L_total_klb.initialize();
  #endif
  L_total_knum_yr.allocate(styr,endyr,"L_total_knum_yr");
  #ifndef NO_AD_INITIALIZE
    L_total_knum_yr.initialize();
  #endif
  L_total_klb_yr.allocate(styr,endyr,"L_total_klb_yr");
  #ifndef NO_AD_INITIALIZE
    L_total_klb_yr.initialize();
  #endif
  D_cH_num.allocate(styr,endyr,1,nages,"D_cH_num");
  #ifndef NO_AD_INITIALIZE
    D_cH_num.initialize();
  #endif
  D_cH_klb.allocate(styr,endyr,1,nages,"D_cH_klb");
  #ifndef NO_AD_INITIALIZE
    D_cH_klb.initialize();
  #endif
  obs_cH_D.allocate(styr_cH_D,endyr_cH_D,"obs_cH_D");
  #ifndef NO_AD_INITIALIZE
    obs_cH_D.initialize();
  #endif
  pred_cH_D_knum.allocate(styr,endyr,"pred_cH_D_knum");
  #ifndef NO_AD_INITIALIZE
    pred_cH_D_knum.initialize();
  #endif
  pred_cH_D_klb.allocate(styr,endyr,"pred_cH_D_klb");
  #ifndef NO_AD_INITIALIZE
    pred_cH_D_klb.initialize();
  #endif
  D_HB_num.allocate(styr,endyr,1,nages,"D_HB_num");
  #ifndef NO_AD_INITIALIZE
    D_HB_num.initialize();
  #endif
  D_HB_klb.allocate(styr,endyr,1,nages,"D_HB_klb");
  #ifndef NO_AD_INITIALIZE
    D_HB_klb.initialize();
  #endif
  obs_HB_D.allocate(styr_HB_D,endyr_HB_D,"obs_HB_D");
  #ifndef NO_AD_INITIALIZE
    obs_HB_D.initialize();
  #endif
  pred_HB_D_knum.allocate(styr,endyr,"pred_HB_D_knum");
  #ifndef NO_AD_INITIALIZE
    pred_HB_D_knum.initialize();
  #endif
  pred_HB_D_klb.allocate(styr,endyr,"pred_HB_D_klb");
  #ifndef NO_AD_INITIALIZE
    pred_HB_D_klb.initialize();
  #endif
  D_GR_num.allocate(styr,endyr,1,nages,"D_GR_num");
  #ifndef NO_AD_INITIALIZE
    D_GR_num.initialize();
  #endif
  D_GR_klb.allocate(styr,endyr,1,nages,"D_GR_klb");
  #ifndef NO_AD_INITIALIZE
    D_GR_klb.initialize();
  #endif
  obs_GR_D.allocate(styr_GR_D,endyr_GR_D,"obs_GR_D");
  #ifndef NO_AD_INITIALIZE
    obs_GR_D.initialize();
  #endif
  pred_GR_D_knum.allocate(styr,endyr,"pred_GR_D_knum");
  #ifndef NO_AD_INITIALIZE
    pred_GR_D_knum.initialize();
  #endif
  pred_GR_D_klb.allocate(styr,endyr,"pred_GR_D_klb");
  #ifndef NO_AD_INITIALIZE
    pred_GR_D_klb.initialize();
  #endif
  D_total_num.allocate(styr,endyr,1,nages,"D_total_num");
  #ifndef NO_AD_INITIALIZE
    D_total_num.initialize();
  #endif
  D_total_klb.allocate(styr,endyr,1,nages,"D_total_klb");
  #ifndef NO_AD_INITIALIZE
    D_total_klb.initialize();
  #endif
  D_total_knum_yr.allocate(styr,endyr,"D_total_knum_yr");
  #ifndef NO_AD_INITIALIZE
    D_total_knum_yr.initialize();
  #endif
  D_total_klb_yr.allocate(styr,endyr,"D_total_klb_yr");
  #ifndef NO_AD_INITIALIZE
    D_total_klb_yr.initialize();
  #endif
  Dmort_cH.allocate("Dmort_cH");
  #ifndef NO_AD_INITIALIZE
  Dmort_cH.initialize();
  #endif
  Dmort_HB.allocate("Dmort_HB");
  #ifndef NO_AD_INITIALIZE
  Dmort_HB.initialize();
  #endif
  Dmort_GR.allocate("Dmort_GR");
  #ifndef NO_AD_INITIALIZE
  Dmort_GR.initialize();
  #endif
  F_cH_prop.allocate("F_cH_prop");
  #ifndef NO_AD_INITIALIZE
  F_cH_prop.initialize();
  #endif
  F_cO_prop.allocate("F_cO_prop");
  #ifndef NO_AD_INITIALIZE
  F_cO_prop.initialize();
  #endif
  F_HB_prop.allocate("F_HB_prop");
  #ifndef NO_AD_INITIALIZE
  F_HB_prop.initialize();
  #endif
  F_GR_prop.allocate("F_GR_prop");
  #ifndef NO_AD_INITIALIZE
  F_GR_prop.initialize();
  #endif
  F_cH_D_prop.allocate("F_cH_D_prop");
  #ifndef NO_AD_INITIALIZE
  F_cH_D_prop.initialize();
  #endif
  F_HB_D_prop.allocate("F_HB_D_prop");
  #ifndef NO_AD_INITIALIZE
  F_HB_D_prop.initialize();
  #endif
  F_GR_D_prop.allocate("F_GR_D_prop");
  #ifndef NO_AD_INITIALIZE
  F_GR_D_prop.initialize();
  #endif
  F_temp_sum.allocate("F_temp_sum");
  #ifndef NO_AD_INITIALIZE
  F_temp_sum.initialize();
  #endif
  F_end.allocate(1,nages,"F_end");
  #ifndef NO_AD_INITIALIZE
    F_end.initialize();
  #endif
  F_end_L.allocate(1,nages,"F_end_L");
  #ifndef NO_AD_INITIALIZE
    F_end_L.initialize();
  #endif
  F_end_D.allocate(1,nages,"F_end_D");
  #ifndef NO_AD_INITIALIZE
    F_end_D.initialize();
  #endif
  F_end_apex.allocate("F_end_apex");
  #ifndef NO_AD_INITIALIZE
  F_end_apex.initialize();
  #endif
  SSB_msy_out.allocate("SSB_msy_out");
  #ifndef NO_AD_INITIALIZE
  SSB_msy_out.initialize();
  #endif
  F_msy_out.allocate("F_msy_out");
  #ifndef NO_AD_INITIALIZE
  F_msy_out.initialize();
  #endif
  msy_klb_out.allocate("msy_klb_out");
  #ifndef NO_AD_INITIALIZE
  msy_klb_out.initialize();
  #endif
  msy_knum_out.allocate("msy_knum_out");
  #ifndef NO_AD_INITIALIZE
  msy_knum_out.initialize();
  #endif
  D_msy_klb_out.allocate("D_msy_klb_out");
  #ifndef NO_AD_INITIALIZE
  D_msy_klb_out.initialize();
  #endif
  D_msy_knum_out.allocate("D_msy_knum_out");
  #ifndef NO_AD_INITIALIZE
  D_msy_knum_out.initialize();
  #endif
  B_msy_out.allocate("B_msy_out");
  #ifndef NO_AD_INITIALIZE
  B_msy_out.initialize();
  #endif
  R_msy_out.allocate("R_msy_out");
  #ifndef NO_AD_INITIALIZE
  R_msy_out.initialize();
  #endif
  spr_msy_out.allocate("spr_msy_out");
  #ifndef NO_AD_INITIALIZE
  spr_msy_out.initialize();
  #endif
  F20_dum.allocate("F20_dum");
  #ifndef NO_AD_INITIALIZE
  F20_dum.initialize();
  #endif
  F30_dum.allocate("F30_dum");
  #ifndef NO_AD_INITIALIZE
  F30_dum.initialize();
  #endif
  F40_dum.allocate("F40_dum");
  #ifndef NO_AD_INITIALIZE
  F40_dum.initialize();
  #endif
  F20_out.allocate("F20_out");
  #ifndef NO_AD_INITIALIZE
  F20_out.initialize();
  #endif
  F30_out.allocate("F30_out");
  #ifndef NO_AD_INITIALIZE
  F30_out.initialize();
  #endif
  F40_out.allocate("F40_out");
  #ifndef NO_AD_INITIALIZE
  F40_out.initialize();
  #endif
  SSB_F30_out.allocate("SSB_F30_out");
  #ifndef NO_AD_INITIALIZE
  SSB_F30_out.initialize();
  #endif
  B_F30_out.allocate("B_F30_out");
  #ifndef NO_AD_INITIALIZE
  B_F30_out.initialize();
  #endif
  R_F30_out.allocate("R_F30_out");
  #ifndef NO_AD_INITIALIZE
  R_F30_out.initialize();
  #endif
  L_F30_knum_out.allocate("L_F30_knum_out");
  #ifndef NO_AD_INITIALIZE
  L_F30_knum_out.initialize();
  #endif
  L_F30_klb_out.allocate("L_F30_klb_out");
  #ifndef NO_AD_INITIALIZE
  L_F30_klb_out.initialize();
  #endif
  D_F30_knum_out.allocate("D_F30_knum_out");
  #ifndef NO_AD_INITIALIZE
  D_F30_knum_out.initialize();
  #endif
  D_F30_klb_out.allocate("D_F30_klb_out");
  #ifndef NO_AD_INITIALIZE
  D_F30_klb_out.initialize();
  #endif
  rec_mean.allocate("rec_mean");
  #ifndef NO_AD_INITIALIZE
  rec_mean.initialize();
  #endif
  N_age_msy.allocate(1,nages,"N_age_msy");
  #ifndef NO_AD_INITIALIZE
    N_age_msy.initialize();
  #endif
  N_age_msy_spawn.allocate(1,nages,"N_age_msy_spawn");
  #ifndef NO_AD_INITIALIZE
    N_age_msy_spawn.initialize();
  #endif
  L_age_msy.allocate(1,nages,"L_age_msy");
  #ifndef NO_AD_INITIALIZE
    L_age_msy.initialize();
  #endif
  D_age_msy.allocate(1,nages,"D_age_msy");
  #ifndef NO_AD_INITIALIZE
    D_age_msy.initialize();
  #endif
  Z_age_msy.allocate(1,nages,"Z_age_msy");
  #ifndef NO_AD_INITIALIZE
    Z_age_msy.initialize();
  #endif
  F_L_age_msy.allocate(1,nages,"F_L_age_msy");
  #ifndef NO_AD_INITIALIZE
    F_L_age_msy.initialize();
  #endif
  F_D_age_msy.allocate(1,nages,"F_D_age_msy");
  #ifndef NO_AD_INITIALIZE
    F_D_age_msy.initialize();
  #endif
  F_msy.allocate(1,n_iter_msy,"F_msy");
  #ifndef NO_AD_INITIALIZE
    F_msy.initialize();
  #endif
  spr_msy.allocate(1,n_iter_msy,"spr_msy");
  #ifndef NO_AD_INITIALIZE
    spr_msy.initialize();
  #endif
  R_eq.allocate(1,n_iter_msy,"R_eq");
  #ifndef NO_AD_INITIALIZE
    R_eq.initialize();
  #endif
  L_eq_klb.allocate(1,n_iter_msy,"L_eq_klb");
  #ifndef NO_AD_INITIALIZE
    L_eq_klb.initialize();
  #endif
  L_eq_knum.allocate(1,n_iter_msy,"L_eq_knum");
  #ifndef NO_AD_INITIALIZE
    L_eq_knum.initialize();
  #endif
  D_eq_klb.allocate(1,n_iter_msy,"D_eq_klb");
  #ifndef NO_AD_INITIALIZE
    D_eq_klb.initialize();
  #endif
  D_eq_knum.allocate(1,n_iter_msy,"D_eq_knum");
  #ifndef NO_AD_INITIALIZE
    D_eq_knum.initialize();
  #endif
  SSB_eq.allocate(1,n_iter_msy,"SSB_eq");
  #ifndef NO_AD_INITIALIZE
    SSB_eq.initialize();
  #endif
  B_eq.allocate(1,n_iter_msy,"B_eq");
  #ifndef NO_AD_INITIALIZE
    B_eq.initialize();
  #endif
  FdF_msy.allocate(styr,endyr,"FdF_msy");
  #ifndef NO_AD_INITIALIZE
    FdF_msy.initialize();
  #endif
  FdF30.allocate(styr,endyr,"FdF30");
  #ifndef NO_AD_INITIALIZE
    FdF30.initialize();
  #endif
  SdSSB_msy.allocate(styr,endyr,"SdSSB_msy");
  #ifndef NO_AD_INITIALIZE
    SdSSB_msy.initialize();
  #endif
  SdSSB_msy_end.allocate("SdSSB_msy_end");
  #ifndef NO_AD_INITIALIZE
  SdSSB_msy_end.initialize();
  #endif
  FdF_msy_end.allocate("FdF_msy_end");
  #ifndef NO_AD_INITIALIZE
  FdF_msy_end.initialize();
  #endif
  FdF_msy_end_mean.allocate("FdF_msy_end_mean");
  #ifndef NO_AD_INITIALIZE
  FdF_msy_end_mean.initialize();
  #endif
  SdSSB_F30.allocate(styr,endyr,"SdSSB_F30");
  #ifndef NO_AD_INITIALIZE
    SdSSB_F30.initialize();
  #endif
  Sdmsst_F30.allocate(styr,endyr,"Sdmsst_F30");
  #ifndef NO_AD_INITIALIZE
    Sdmsst_F30.initialize();
  #endif
  SdSSB_F30_end.allocate("SdSSB_F30_end");
  #ifndef NO_AD_INITIALIZE
  SdSSB_F30_end.initialize();
  #endif
  Sdmsst_F30_end.allocate("Sdmsst_F30_end");
  #ifndef NO_AD_INITIALIZE
  Sdmsst_F30_end.initialize();
  #endif
  FdF30_end_mean.allocate("FdF30_end_mean");
  #ifndef NO_AD_INITIALIZE
  FdF30_end_mean.initialize();
  #endif
  Fend_mean_temp.allocate("Fend_mean_temp");
  #ifndef NO_AD_INITIALIZE
  Fend_mean_temp.initialize();
  #endif
  Fend_mean.allocate("Fend_mean");
  #ifndef NO_AD_INITIALIZE
  Fend_mean.initialize();
  #endif
  L_age_F30.allocate(1,nages,"L_age_F30");
  #ifndef NO_AD_INITIALIZE
    L_age_F30.initialize();
  #endif
  D_age_F30.allocate(1,nages,"D_age_F30");
  #ifndef NO_AD_INITIALIZE
    D_age_F30.initialize();
  #endif
  wgt_wgted_L_klb.allocate(1,nages,"wgt_wgted_L_klb");
  #ifndef NO_AD_INITIALIZE
    wgt_wgted_L_klb.initialize();
  #endif
  wgt_wgted_D_klb.allocate(1,nages,"wgt_wgted_D_klb");
  #ifndef NO_AD_INITIALIZE
    wgt_wgted_D_klb.initialize();
  #endif
  wgt_wgted_L_denom.allocate("wgt_wgted_L_denom");
  #ifndef NO_AD_INITIALIZE
  wgt_wgted_L_denom.initialize();
  #endif
  wgt_wgted_D_denom.allocate("wgt_wgted_D_denom");
  #ifndef NO_AD_INITIALIZE
  wgt_wgted_D_denom.initialize();
  #endif
  iter_inc_msy.allocate("iter_inc_msy");
  #ifndef NO_AD_INITIALIZE
  iter_inc_msy.initialize();
  #endif
  M.allocate(1,nages,"M");
  #ifndef NO_AD_INITIALIZE
    M.initialize();
  #endif
  M_constant.allocate(M_constant_LO,M_constant_HI,M_constant_PH,"M_constant");
  M_constant_out.allocate(1,8,"M_constant_out");
  #ifndef NO_AD_INITIALIZE
    M_constant_out.initialize();
  #endif
  smsy2msstM.allocate("smsy2msstM");
  #ifndef NO_AD_INITIALIZE
  smsy2msstM.initialize();
  #endif
  smsy2msst75.allocate("smsy2msst75");
  #ifndef NO_AD_INITIALIZE
  smsy2msst75.initialize();
  #endif
  F.allocate(styr,endyr,1,nages,"F");
  #ifndef NO_AD_INITIALIZE
    F.initialize();
  #endif
  Fsum.allocate(styr,endyr,"Fsum");
  #ifndef NO_AD_INITIALIZE
    Fsum.initialize();
  #endif
  Fapex.allocate(styr,endyr,"Fapex");
  #ifndef NO_AD_INITIALIZE
    Fapex.initialize();
  #endif
  Z.allocate(styr,endyr,1,nages,"Z");
  #ifndef NO_AD_INITIALIZE
    Z.initialize();
  #endif
  log_avg_F_cH.allocate(log_avg_F_cH_LO,log_avg_F_cH_HI,log_avg_F_cH_PH,"log_avg_F_cH");
  log_avg_F_cH_out.allocate(1,8,"log_avg_F_cH_out");
  #ifndef NO_AD_INITIALIZE
    log_avg_F_cH_out.initialize();
  #endif
  log_F_dev_cH.allocate(styr_cH_L,endyr_cH_L,log_F_dev_cH_LO,log_F_dev_cH_HI,log_F_dev_cH_PH,"log_F_dev_cH");
  log_F_dev_cH_out.allocate(styr_cH_L,endyr_cH_L,"log_F_dev_cH_out");
  #ifndef NO_AD_INITIALIZE
    log_F_dev_cH_out.initialize();
  #endif
  F_cH.allocate(styr,endyr,1,nages,"F_cH");
  #ifndef NO_AD_INITIALIZE
    F_cH.initialize();
  #endif
  F_cH_out.allocate(styr,endyr,"F_cH_out");
  #ifndef NO_AD_INITIALIZE
    F_cH_out.initialize();
  #endif
  log_F_dev_init_cH.allocate("log_F_dev_init_cH");
  #ifndef NO_AD_INITIALIZE
  log_F_dev_init_cH.initialize();
  #endif
  log_F_dev_end_cH.allocate("log_F_dev_end_cH");
  #ifndef NO_AD_INITIALIZE
  log_F_dev_end_cH.initialize();
  #endif
  log_avg_F_cO.allocate(log_avg_F_cO_LO,log_avg_F_cO_HI,log_avg_F_cO_PH,"log_avg_F_cO");
  log_avg_F_cO_out.allocate(1,8,"log_avg_F_cO_out");
  #ifndef NO_AD_INITIALIZE
    log_avg_F_cO_out.initialize();
  #endif
  log_F_dev_cO.allocate(styr_cO_L,endyr_cO_L,log_F_dev_cO_LO,log_F_dev_cO_HI,log_F_dev_cO_PH,"log_F_dev_cO");
  log_F_dev_cO_out.allocate(styr_cO_L,endyr_cO_L,"log_F_dev_cO_out");
  #ifndef NO_AD_INITIALIZE
    log_F_dev_cO_out.initialize();
  #endif
  F_cO.allocate(styr,endyr,1,nages,"F_cO");
  #ifndef NO_AD_INITIALIZE
    F_cO.initialize();
  #endif
  F_cO_out.allocate(styr,endyr,"F_cO_out");
  #ifndef NO_AD_INITIALIZE
    F_cO_out.initialize();
  #endif
  log_F_dev_init_cO.allocate("log_F_dev_init_cO");
  #ifndef NO_AD_INITIALIZE
  log_F_dev_init_cO.initialize();
  #endif
  log_F_dev_end_cO.allocate("log_F_dev_end_cO");
  #ifndef NO_AD_INITIALIZE
  log_F_dev_end_cO.initialize();
  #endif
  log_avg_F_HB.allocate(log_avg_F_HB_LO,log_avg_F_HB_HI,log_avg_F_HB_PH,"log_avg_F_HB");
  log_avg_F_HB_out.allocate(1,8,"log_avg_F_HB_out");
  #ifndef NO_AD_INITIALIZE
    log_avg_F_HB_out.initialize();
  #endif
  log_F_dev_HB.allocate(styr_HB_L,endyr_HB_L,log_F_dev_HB_LO,log_F_dev_HB_HI,log_F_dev_HB_PH,"log_F_dev_HB");
  log_F_dev_HB_out.allocate(styr_HB_L,endyr_HB_L,"log_F_dev_HB_out");
  #ifndef NO_AD_INITIALIZE
    log_F_dev_HB_out.initialize();
  #endif
  F_HB.allocate(styr,endyr,1,nages,"F_HB");
  #ifndef NO_AD_INITIALIZE
    F_HB.initialize();
  #endif
  F_HB_out.allocate(styr,endyr,"F_HB_out");
  #ifndef NO_AD_INITIALIZE
    F_HB_out.initialize();
  #endif
  log_F_dev_init_HB.allocate("log_F_dev_init_HB");
  #ifndef NO_AD_INITIALIZE
  log_F_dev_init_HB.initialize();
  #endif
  log_F_dev_end_HB.allocate("log_F_dev_end_HB");
  #ifndef NO_AD_INITIALIZE
  log_F_dev_end_HB.initialize();
  #endif
  log_avg_F_GR.allocate(log_avg_F_GR_LO,log_avg_F_GR_HI,log_avg_F_GR_PH,"log_avg_F_GR");
  log_avg_F_GR_out.allocate(1,8,"log_avg_F_GR_out");
  #ifndef NO_AD_INITIALIZE
    log_avg_F_GR_out.initialize();
  #endif
  log_F_dev_GR.allocate(styr_GR_L,endyr_GR_L,log_F_dev_GR_LO,log_F_dev_GR_HI,log_F_dev_GR_PH,"log_F_dev_GR");
  log_F_dev_GR_out.allocate(styr_GR_L,endyr_GR_L,"log_F_dev_GR_out");
  #ifndef NO_AD_INITIALIZE
    log_F_dev_GR_out.initialize();
  #endif
  F_GR.allocate(styr,endyr,1,nages,"F_GR");
  #ifndef NO_AD_INITIALIZE
    F_GR.initialize();
  #endif
  F_GR_out.allocate(styr,endyr,"F_GR_out");
  #ifndef NO_AD_INITIALIZE
    F_GR_out.initialize();
  #endif
  log_F_dev_init_GR.allocate("log_F_dev_init_GR");
  #ifndef NO_AD_INITIALIZE
  log_F_dev_init_GR.initialize();
  #endif
  log_F_dev_end_GR.allocate("log_F_dev_end_GR");
  #ifndef NO_AD_INITIALIZE
  log_F_dev_end_GR.initialize();
  #endif
  log_avg_F_cH_D.allocate(log_avg_F_cH_D_LO,log_avg_F_cH_D_HI,log_avg_F_cH_D_PH,"log_avg_F_cH_D");
  log_avg_F_cH_D_out.allocate(1,8,"log_avg_F_cH_D_out");
  #ifndef NO_AD_INITIALIZE
    log_avg_F_cH_D_out.initialize();
  #endif
  log_F_dev_cH_D.allocate(styr_cH_D,endyr_cH_D,log_F_dev_cH_D_LO,log_F_dev_cH_D_HI,log_F_dev_cH_D_PH,"log_F_dev_cH_D");
  log_F_dev_cH_D_out.allocate(styr_cH_D,endyr_cH_D,"log_F_dev_cH_D_out");
  #ifndef NO_AD_INITIALIZE
    log_F_dev_cH_D_out.initialize();
  #endif
  F_cH_D.allocate(styr,endyr,1,nages,"F_cH_D");
  #ifndef NO_AD_INITIALIZE
    F_cH_D.initialize();
  #endif
  F_cH_D_out.allocate(styr,endyr,"F_cH_D_out");
  #ifndef NO_AD_INITIALIZE
    F_cH_D_out.initialize();
  #endif
  log_F_dev_end_cH_D.allocate("log_F_dev_end_cH_D");
  #ifndef NO_AD_INITIALIZE
  log_F_dev_end_cH_D.initialize();
  #endif
  log_F_avgdev_cH_D.allocate("log_F_avgdev_cH_D");
  #ifndef NO_AD_INITIALIZE
  log_F_avgdev_cH_D.initialize();
  #endif
  log_avg_F_HB_D.allocate(log_avg_F_HB_D_LO,log_avg_F_HB_D_HI,log_avg_F_HB_D_PH,"log_avg_F_HB_D");
  log_avg_F_HB_D_out.allocate(1,8,"log_avg_F_HB_D_out");
  #ifndef NO_AD_INITIALIZE
    log_avg_F_HB_D_out.initialize();
  #endif
  log_F_dev_HB_D.allocate(styr_HB_D,endyr_HB_D,log_F_dev_HB_D_LO,log_F_dev_HB_D_HI,log_F_dev_HB_D_PH,"log_F_dev_HB_D");
  log_F_dev_HB_D_out.allocate(styr_HB_D,endyr_HB_D,"log_F_dev_HB_D_out");
  #ifndef NO_AD_INITIALIZE
    log_F_dev_HB_D_out.initialize();
  #endif
  F_HB_D.allocate(styr,endyr,1,nages,"F_HB_D");
  #ifndef NO_AD_INITIALIZE
    F_HB_D.initialize();
  #endif
  F_HB_D_out.allocate(styr,endyr,"F_HB_D_out");
  #ifndef NO_AD_INITIALIZE
    F_HB_D_out.initialize();
  #endif
  log_F_dev_end_HB_D.allocate("log_F_dev_end_HB_D");
  #ifndef NO_AD_INITIALIZE
  log_F_dev_end_HB_D.initialize();
  #endif
  log_F_avgdev_HB_D.allocate("log_F_avgdev_HB_D");
  #ifndef NO_AD_INITIALIZE
  log_F_avgdev_HB_D.initialize();
  #endif
  log_avg_F_GR_D.allocate(log_avg_F_GR_D_LO,log_avg_F_GR_D_HI,log_avg_F_GR_D_PH,"log_avg_F_GR_D");
  log_avg_F_GR_D_out.allocate(1,8,"log_avg_F_GR_D_out");
  #ifndef NO_AD_INITIALIZE
    log_avg_F_GR_D_out.initialize();
  #endif
  log_F_dev_GR_D.allocate(styr_GR_D,endyr_GR_D,log_F_dev_GR_D_LO,log_F_dev_GR_D_HI,log_F_dev_GR_D_PH,"log_F_dev_GR_D");
  log_F_dev_GR_D_out.allocate(styr_GR_D,endyr_GR_D,"log_F_dev_GR_D_out");
  #ifndef NO_AD_INITIALIZE
    log_F_dev_GR_D_out.initialize();
  #endif
  F_GR_D.allocate(styr,endyr,1,nages,"F_GR_D");
  #ifndef NO_AD_INITIALIZE
    F_GR_D.initialize();
  #endif
  F_GR_D_out.allocate(styr,endyr,"F_GR_D_out");
  #ifndef NO_AD_INITIALIZE
    F_GR_D_out.initialize();
  #endif
  log_F_dev_end_GR_D.allocate("log_F_dev_end_GR_D");
  #ifndef NO_AD_INITIALIZE
  log_F_dev_end_GR_D.initialize();
  #endif
  log_F_avgdev_GR_D.allocate("log_F_avgdev_GR_D");
  #ifndef NO_AD_INITIALIZE
  log_F_avgdev_GR_D.initialize();
  #endif
  N_age_spr.allocate(1,nages,"N_age_spr");
  #ifndef NO_AD_INITIALIZE
    N_age_spr.initialize();
  #endif
  N_age_spr_spawn.allocate(1,nages,"N_age_spr_spawn");
  #ifndef NO_AD_INITIALIZE
    N_age_spr_spawn.initialize();
  #endif
  L_age_spr.allocate(1,nages,"L_age_spr");
  #ifndef NO_AD_INITIALIZE
    L_age_spr.initialize();
  #endif
  Z_age_spr.allocate(1,nages,"Z_age_spr");
  #ifndef NO_AD_INITIALIZE
    Z_age_spr.initialize();
  #endif
  spr_static.allocate(styr,endyr,"spr_static");
  #ifndef NO_AD_INITIALIZE
    spr_static.initialize();
  #endif
  F_L_age_spr.allocate(1,nages,"F_L_age_spr");
  #ifndef NO_AD_INITIALIZE
    F_L_age_spr.initialize();
  #endif
  F_D_age_spr.allocate(1,nages,"F_D_age_spr");
  #ifndef NO_AD_INITIALIZE
    F_D_age_spr.initialize();
  #endif
  F_spr.allocate(1,n_iter_spr,"F_spr");
  #ifndef NO_AD_INITIALIZE
    F_spr.initialize();
  #endif
  spr_spr.allocate(1,n_iter_spr,"spr_spr");
  #ifndef NO_AD_INITIALIZE
    spr_spr.initialize();
  #endif
  spr_ratio.allocate(1,n_iter_spr,"spr_ratio");
  #ifndef NO_AD_INITIALIZE
    spr_ratio.initialize();
  #endif
  L_spr.allocate(1,n_iter_spr,"L_spr");
  #ifndef NO_AD_INITIALIZE
    L_spr.initialize();
  #endif
  N_spr_F0.allocate(1,nages,"N_spr_F0");
  #ifndef NO_AD_INITIALIZE
    N_spr_F0.initialize();
  #endif
  N_bpr_F0.allocate(1,nages,"N_bpr_F0");
  #ifndef NO_AD_INITIALIZE
    N_bpr_F0.initialize();
  #endif
  N_spr_initial.allocate(1,nages,"N_spr_initial");
  #ifndef NO_AD_INITIALIZE
    N_spr_initial.initialize();
  #endif
  N_initial_eq.allocate(1,nages,"N_initial_eq");
  #ifndef NO_AD_INITIALIZE
    N_initial_eq.initialize();
  #endif
  F_initial.allocate(1,nages,"F_initial");
  #ifndef NO_AD_INITIALIZE
    F_initial.initialize();
  #endif
  Z_initial.allocate(1,nages,"Z_initial");
  #ifndef NO_AD_INITIALIZE
    Z_initial.initialize();
  #endif
  spr_initial.allocate("spr_initial");
  #ifndef NO_AD_INITIALIZE
  spr_initial.initialize();
  #endif
  spr_F0.allocate("spr_F0");
  #ifndef NO_AD_INITIALIZE
  spr_F0.initialize();
  #endif
  bpr_F0.allocate("bpr_F0");
  #ifndef NO_AD_INITIALIZE
  bpr_F0.initialize();
  #endif
  iter_inc_spr.allocate("iter_inc_spr");
  #ifndef NO_AD_INITIALIZE
  iter_inc_spr.initialize();
  #endif
  sdnr_lc_cH.allocate("sdnr_lc_cH");
  #ifndef NO_AD_INITIALIZE
  sdnr_lc_cH.initialize();
  #endif
  sdnr_lc_cO.allocate("sdnr_lc_cO");
  #ifndef NO_AD_INITIALIZE
  sdnr_lc_cO.initialize();
  #endif
  sdnr_lc_HB.allocate("sdnr_lc_HB");
  #ifndef NO_AD_INITIALIZE
  sdnr_lc_HB.initialize();
  #endif
  sdnr_lc_HB_D.allocate("sdnr_lc_HB_D");
  #ifndef NO_AD_INITIALIZE
  sdnr_lc_HB_D.initialize();
  #endif
  sdnr_lc_GR.allocate("sdnr_lc_GR");
  #ifndef NO_AD_INITIALIZE
  sdnr_lc_GR.initialize();
  #endif
  sdnr_lc_CVT.allocate("sdnr_lc_CVT");
  #ifndef NO_AD_INITIALIZE
  sdnr_lc_CVT.initialize();
  #endif
  sdnr_ac_cH.allocate("sdnr_ac_cH");
  #ifndef NO_AD_INITIALIZE
  sdnr_ac_cH.initialize();
  #endif
  sdnr_ac_HB.allocate("sdnr_ac_HB");
  #ifndef NO_AD_INITIALIZE
  sdnr_ac_HB.initialize();
  #endif
  sdnr_ac_GR.allocate("sdnr_ac_GR");
  #ifndef NO_AD_INITIALIZE
  sdnr_ac_GR.initialize();
  #endif
  sdnr_ac_CVT.allocate("sdnr_ac_CVT");
  #ifndef NO_AD_INITIALIZE
  sdnr_ac_CVT.initialize();
  #endif
  sdnr_I_cH.allocate("sdnr_I_cH");
  #ifndef NO_AD_INITIALIZE
  sdnr_I_cH.initialize();
  #endif
  sdnr_I_HB.allocate("sdnr_I_HB");
  #ifndef NO_AD_INITIALIZE
  sdnr_I_HB.initialize();
  #endif
  sdnr_I_CVT.allocate("sdnr_I_CVT");
  #ifndef NO_AD_INITIALIZE
  sdnr_I_CVT.initialize();
  #endif
  w_L.allocate("w_L");
  #ifndef NO_AD_INITIALIZE
  w_L.initialize();
  #endif
  w_D.allocate("w_D");
  #ifndef NO_AD_INITIALIZE
  w_D.initialize();
  #endif
  w_I_cH.allocate("w_I_cH");
  #ifndef NO_AD_INITIALIZE
  w_I_cH.initialize();
  #endif
  w_I_HB.allocate("w_I_HB");
  #ifndef NO_AD_INITIALIZE
  w_I_HB.initialize();
  #endif
  w_I_CVT.allocate("w_I_CVT");
  #ifndef NO_AD_INITIALIZE
  w_I_CVT.initialize();
  #endif
  w_lc_cH.allocate("w_lc_cH");
  #ifndef NO_AD_INITIALIZE
  w_lc_cH.initialize();
  #endif
  w_lc_cO.allocate("w_lc_cO");
  #ifndef NO_AD_INITIALIZE
  w_lc_cO.initialize();
  #endif
  w_lc_HB.allocate("w_lc_HB");
  #ifndef NO_AD_INITIALIZE
  w_lc_HB.initialize();
  #endif
  w_lc_HB_D.allocate("w_lc_HB_D");
  #ifndef NO_AD_INITIALIZE
  w_lc_HB_D.initialize();
  #endif
  w_lc_GR.allocate("w_lc_GR");
  #ifndef NO_AD_INITIALIZE
  w_lc_GR.initialize();
  #endif
  w_lc_CVT.allocate("w_lc_CVT");
  #ifndef NO_AD_INITIALIZE
  w_lc_CVT.initialize();
  #endif
  w_ac_cH.allocate("w_ac_cH");
  #ifndef NO_AD_INITIALIZE
  w_ac_cH.initialize();
  #endif
  w_ac_HB.allocate("w_ac_HB");
  #ifndef NO_AD_INITIALIZE
  w_ac_HB.initialize();
  #endif
  w_ac_GR.allocate("w_ac_GR");
  #ifndef NO_AD_INITIALIZE
  w_ac_GR.initialize();
  #endif
  w_ac_CVT.allocate("w_ac_CVT");
  #ifndef NO_AD_INITIALIZE
  w_ac_CVT.initialize();
  #endif
  w_Nage_init.allocate("w_Nage_init");
  #ifndef NO_AD_INITIALIZE
  w_Nage_init.initialize();
  #endif
  w_rec.allocate("w_rec");
  #ifndef NO_AD_INITIALIZE
  w_rec.initialize();
  #endif
  w_rec_early.allocate("w_rec_early");
  #ifndef NO_AD_INITIALIZE
  w_rec_early.initialize();
  #endif
  w_rec_end.allocate("w_rec_end");
  #ifndef NO_AD_INITIALIZE
  w_rec_end.initialize();
  #endif
  w_fullF.allocate("w_fullF");
  #ifndef NO_AD_INITIALIZE
  w_fullF.initialize();
  #endif
  w_Ftune.allocate("w_Ftune");
  #ifndef NO_AD_INITIALIZE
  w_Ftune.initialize();
  #endif
  f_cH_L.allocate("f_cH_L");
  #ifndef NO_AD_INITIALIZE
  f_cH_L.initialize();
  #endif
  f_cO_L.allocate("f_cO_L");
  #ifndef NO_AD_INITIALIZE
  f_cO_L.initialize();
  #endif
  f_HB_L.allocate("f_HB_L");
  #ifndef NO_AD_INITIALIZE
  f_HB_L.initialize();
  #endif
  f_GR_L.allocate("f_GR_L");
  #ifndef NO_AD_INITIALIZE
  f_GR_L.initialize();
  #endif
  f_cH_D.allocate("f_cH_D");
  #ifndef NO_AD_INITIALIZE
  f_cH_D.initialize();
  #endif
  f_HB_D.allocate("f_HB_D");
  #ifndef NO_AD_INITIALIZE
  f_HB_D.initialize();
  #endif
  f_GR_D.allocate("f_GR_D");
  #ifndef NO_AD_INITIALIZE
  f_GR_D.initialize();
  #endif
  f_cH_cpue.allocate("f_cH_cpue");
  #ifndef NO_AD_INITIALIZE
  f_cH_cpue.initialize();
  #endif
  f_HB_cpue.allocate("f_HB_cpue");
  #ifndef NO_AD_INITIALIZE
  f_HB_cpue.initialize();
  #endif
  f_CVT_cpue.allocate("f_CVT_cpue");
  #ifndef NO_AD_INITIALIZE
  f_CVT_cpue.initialize();
  #endif
  f_HB_RWq_cpue.allocate("f_HB_RWq_cpue");
  #ifndef NO_AD_INITIALIZE
  f_HB_RWq_cpue.initialize();
  #endif
  f_cH_RWq_cpue.allocate("f_cH_RWq_cpue");
  #ifndef NO_AD_INITIALIZE
  f_cH_RWq_cpue.initialize();
  #endif
  f_cH_lenc.allocate("f_cH_lenc");
  #ifndef NO_AD_INITIALIZE
  f_cH_lenc.initialize();
  #endif
  f_cO_lenc.allocate("f_cO_lenc");
  #ifndef NO_AD_INITIALIZE
  f_cO_lenc.initialize();
  #endif
  f_HB_lenc.allocate("f_HB_lenc");
  #ifndef NO_AD_INITIALIZE
  f_HB_lenc.initialize();
  #endif
  f_HB_D_lenc.allocate("f_HB_D_lenc");
  #ifndef NO_AD_INITIALIZE
  f_HB_D_lenc.initialize();
  #endif
  f_GR_lenc.allocate("f_GR_lenc");
  #ifndef NO_AD_INITIALIZE
  f_GR_lenc.initialize();
  #endif
  f_CVT_lenc.allocate("f_CVT_lenc");
  #ifndef NO_AD_INITIALIZE
  f_CVT_lenc.initialize();
  #endif
  f_cH_agec.allocate("f_cH_agec");
  #ifndef NO_AD_INITIALIZE
  f_cH_agec.initialize();
  #endif
  f_HB_agec.allocate("f_HB_agec");
  #ifndef NO_AD_INITIALIZE
  f_HB_agec.initialize();
  #endif
  f_GR_agec.allocate("f_GR_agec");
  #ifndef NO_AD_INITIALIZE
  f_GR_agec.initialize();
  #endif
  f_CVT_agec.allocate("f_CVT_agec");
  #ifndef NO_AD_INITIALIZE
  f_CVT_agec.initialize();
  #endif
  f_Nage_init.allocate("f_Nage_init");
  #ifndef NO_AD_INITIALIZE
  f_Nage_init.initialize();
  #endif
  f_rec_dev.allocate("f_rec_dev");
  #ifndef NO_AD_INITIALIZE
  f_rec_dev.initialize();
  #endif
  f_rec_dev_early.allocate("f_rec_dev_early");
  #ifndef NO_AD_INITIALIZE
  f_rec_dev_early.initialize();
  #endif
  f_rec_dev_end.allocate("f_rec_dev_end");
  #ifndef NO_AD_INITIALIZE
  f_rec_dev_end.initialize();
  #endif
  f_fullF_constraint.allocate("f_fullF_constraint");
  #ifndef NO_AD_INITIALIZE
  f_fullF_constraint.initialize();
  #endif
  f_Ftune.allocate("f_Ftune");
  #ifndef NO_AD_INITIALIZE
  f_Ftune.initialize();
  #endif
  f_priors.allocate("f_priors");
  #ifndef NO_AD_INITIALIZE
  f_priors.initialize();
  #endif
  fval.allocate("fval");
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
  fval_data.allocate("fval_data");
  #ifndef NO_AD_INITIALIZE
  fval_data.initialize();
  #endif
  grad_max.allocate("grad_max");
  #ifndef NO_AD_INITIALIZE
  grad_max.initialize();
  #endif
  denom.allocate("denom");
  #ifndef NO_AD_INITIALIZE
  denom.initialize();
  #endif
  numer.allocate("numer");
  #ifndef NO_AD_INITIALIZE
  numer.initialize();
  #endif
  F_reg_proj.allocate("F_reg_proj");
  #ifndef NO_AD_INITIALIZE
  F_reg_proj.initialize();
  #endif
  F_proj.allocate(styr_proj,endyr_proj,"F_proj");
  #ifndef NO_AD_INITIALIZE
    F_proj.initialize();
  #endif
  L_knum_proj.allocate(styr_proj,endyr_proj,"L_knum_proj");
  #ifndef NO_AD_INITIALIZE
    L_knum_proj.initialize();
  #endif
  L_klb_proj.allocate(styr_proj,endyr_proj,"L_klb_proj");
  #ifndef NO_AD_INITIALIZE
    L_klb_proj.initialize();
  #endif
  D_knum_proj.allocate(styr_proj,endyr_proj,"D_knum_proj");
  #ifndef NO_AD_INITIALIZE
    D_knum_proj.initialize();
  #endif
  D_klb_proj.allocate(styr_proj,endyr_proj,"D_klb_proj");
  #ifndef NO_AD_INITIALIZE
    D_klb_proj.initialize();
  #endif
  B_proj.allocate(styr_proj,endyr_proj,"B_proj");
  #ifndef NO_AD_INITIALIZE
    B_proj.initialize();
  #endif
  SSB_proj.allocate(styr_proj,endyr_proj,"SSB_proj");
  #ifndef NO_AD_INITIALIZE
    SSB_proj.initialize();
  #endif
  R_proj.allocate(styr_proj,endyr_proj,"R_proj");
  #ifndef NO_AD_INITIALIZE
    R_proj.initialize();
  #endif
  FL_age_proj.allocate(1,nages,"FL_age_proj");
  #ifndef NO_AD_INITIALIZE
    FL_age_proj.initialize();
  #endif
  FD_age_proj.allocate(1,nages,"FD_age_proj");
  #ifndef NO_AD_INITIALIZE
    FD_age_proj.initialize();
  #endif
  N_proj.allocate(styr_proj,endyr_proj,1,nages,"N_proj");
  #ifndef NO_AD_INITIALIZE
    N_proj.initialize();
  #endif
  N_spawn_proj.allocate(styr_proj,endyr_proj,1,nages,"N_spawn_proj");
  #ifndef NO_AD_INITIALIZE
    N_spawn_proj.initialize();
  #endif
  Z_proj.allocate(styr_proj,endyr_proj,1,nages,"Z_proj");
  #ifndef NO_AD_INITIALIZE
    Z_proj.initialize();
  #endif
  L_age_proj.allocate(styr_proj,endyr_proj,1,nages,"L_age_proj");
  #ifndef NO_AD_INITIALIZE
    L_age_proj.initialize();
  #endif
  D_age_proj.allocate(styr_proj,endyr_proj,1,nages,"D_age_proj");
  #ifndef NO_AD_INITIALIZE
    D_age_proj.initialize();
  #endif
}

void model_parameters::set_runtime(void)
{
  dvector temp1("{1000, 2000,3000, 5000, 10000, 10000, 10000;}");
  maximum_function_evaluations.allocate(temp1.indexmin(),temp1.indexmax());
  maximum_function_evaluations=temp1;
  dvector temp("{1e-2, 1e-2,1e-3, 1e-3, 1e-3, 1e-4, 1e-4;}");
  convergence_criteria.allocate(temp.indexmin(),temp.indexmax());
  convergence_criteria=temp;
}

void model_parameters::preliminary_calculations(void)
{

#if defined(USE_ADPVM)

  admaster_slave_variable_interface(*this);

#endif
  Dmort_cH=set_Dmort_cH; Dmort_HB=set_Dmort_HB; Dmort_GR=set_Dmort_GR;
  for(iyear=styr_cH_D; iyear<=endyr_cH_D; iyear++)
	{obs_cH_D(iyear)=Dmort_cH*obs_cH_released(iyear);
	}
	
  for(iyear=styr_HB_D; iyear<=endyr_HB_D; iyear++)
	{obs_HB_D(iyear)=Dmort_HB*obs_HB_released(iyear);
	}
	
  for(iyear=styr_GR_D; iyear<=endyr_GR_D; iyear++)
	{obs_GR_D(iyear)=Dmort_GR*obs_GR_released(iyear);
	}
 
 //Population		
  Linf=set_Linf(1);
  K=set_K(1);
  t0=set_t0(1);
  len_cv_val=set_len_cv(1);
  
  M=set_M; 
  M_constant=set_M_constant(1);
  smsy2msstM=1.0-M_constant;
  smsy2msst75=0.75;  
  
  log_R0=set_log_R0(1);
  steep=set_steep(1);
  R_autocorr=set_R_autocorr(1);
  rec_sigma=set_rec_sigma(1);
  
  log_dm_cH_lc=set_log_dm_cH_lc(1);
  log_dm_cO_lc=set_log_dm_cO_lc(1);
  log_dm_HB_lc=set_log_dm_HB_lc(1);
  log_dm_GR_lc=set_log_dm_GR_lc(1);
  log_dm_HB_D_lc=set_log_dm_HB_D_lc(1);
  log_dm_CVT_lc=set_log_dm_CVT_lc(1);
  log_dm_cH_ac=set_log_dm_cH_ac(1);
  log_dm_HB_ac=set_log_dm_HB_ac(1);
  log_dm_GR_ac=set_log_dm_GR_ac(1);
  log_dm_CVT_ac=set_log_dm_CVT_ac(1);
  
  log_q_cH=set_log_q_cH(1);
  log_q_HB=set_log_q_HB(1);
  //log_q_GR=set_log_q_GR(1);
  log_q_CVT=set_log_q_CVT(1);
  
  q_rate=set_q_rate;
  q_rate_fcn_cH=1.0;   
  q_rate_fcn_HB=1.0;   
  //q_rate_fcn_GR=1.0; 
  q_DD_beta=set_q_DD_beta;
  q_DD_fcn=1.0;
  q_RW_log_dev_cH.initialize(); 
  q_RW_log_dev_HB.initialize(); 
  //q_RW_log_dev_GR.initialize();
  q_RW_log_dev_CVT.initialize();
  
   if (set_q_rate_phase<0 & q_rate!=0.0)
  {
    for (iyear=styr_cH_cpue; iyear<=endyr_cH_cpue; iyear++)
      {   if (iyear>styr_cH_cpue & iyear <=2003) 
          {//q_rate_fcn_cH(iyear)=(1.0+q_rate)*q_rate_fcn_cH(iyear-1); //compound
             q_rate_fcn_cH(iyear)=(1.0+(iyear-styr_cH_cpue)*q_rate)*q_rate_fcn_cH(styr_cH_cpue);  //linear
          }
          if (iyear>2003) {q_rate_fcn_cH(iyear)=q_rate_fcn_cH(iyear-1);} 
      }   
    for (iyear=styr_HB_cpue; iyear<=endyr_HB_cpue; iyear++)
      {   if (iyear>styr_HB_cpue & iyear <=2003) 
          {//q_rate_fcn_HB(iyear)=(1.0+q_rate)*q_rate_fcn_HB(iyear-1); //compound
             q_rate_fcn_HB(iyear)=(1.0+(iyear-styr_HB_cpue)*q_rate)*q_rate_fcn_HB(styr_HB_cpue);  //linear
          }
          if (iyear>2003) {q_rate_fcn_HB(iyear)=q_rate_fcn_HB(iyear-1);} 
      }   
    // for (iyear=styr_GR_cpue; iyear<=endyr_GR_cpue; iyear++)
      // {   if (iyear>styr_GR_cpue & iyear <=2003) 
          // {//q_rate_fcn_GR(iyear)=(1.0+q_rate)*q_rate_fcn_GR(iyear-1); //compound
             // q_rate_fcn_GR(iyear)=(1.0+(iyear-styr_GR_cpue)*q_rate)*q_rate_fcn_GR(styr_GR_cpue);  //linear
          // }
          // if (iyear>2003) {q_rate_fcn_GR(iyear)=q_rate_fcn_GR(iyear-1);} 
      // }   
	  
  } //end q_rate conditional      
  w_L=set_w_L;
  w_D=set_w_D;
  
  w_I_cH=set_w_I_cH;
  w_I_HB=set_w_I_HB;
  //w_I_GR=set_w_I_GR;
  w_I_CVT=set_w_I_CVT;
  
  w_lc_cH=set_w_lc_cH;
  w_lc_cO=set_w_lc_cO;  
  w_lc_HB=set_w_lc_HB;
  w_lc_HB_D=set_w_lc_HB_D; 
  w_lc_GR=set_w_lc_GR;
  w_lc_CVT=set_w_lc_CVT;    
    
  w_ac_cH=set_w_ac_cH;
  w_ac_HB=set_w_ac_HB;
  w_ac_GR=set_w_ac_GR; 
  w_ac_CVT=set_w_ac_CVT;  
 
  
  w_Nage_init=set_w_Nage_init;
  w_rec=set_w_rec;
  w_rec_early=set_w_rec_early;
  w_rec_end=set_w_rec_end;
  w_fullF=set_w_fullF;
  w_Ftune=set_w_Ftune;
  log_avg_F_cH=set_log_avg_F_cH(1);
  log_avg_F_cO=set_log_avg_F_cO(1);  
  log_avg_F_HB=set_log_avg_F_HB(1); 
  log_avg_F_GR=set_log_avg_F_GR(1); 
  log_avg_F_cH_D=set_log_avg_F_cH_D(1);
  log_avg_F_HB_D=set_log_avg_F_HB_D(1); 
  log_avg_F_GR_D=set_log_avg_F_GR_D(1); 
    
  log_F_dev_cH=set_log_F_dev_cH_vals;
  log_F_dev_cO=set_log_F_dev_cO_vals;
  log_F_dev_HB=set_log_F_dev_HB_vals;
  log_F_dev_GR=set_log_F_dev_GR_vals;
  log_F_dev_cH_D=set_log_F_dev_cH_D_vals;
  log_F_dev_HB_D=set_log_F_dev_HB_D_vals;
  log_F_dev_GR_D=set_log_F_dev_GR_D_vals;
 
  //selpar_A50_cH1=set_selpar_A50_cH1(1);
  //selpar_slope_cH1=set_selpar_slope_cH1(1);
  selpar_A50_cH2=set_selpar_A50_cH2(1);
  selpar_slope_cH2=set_selpar_slope_cH2(1);
  selpar_A50_cH3=set_selpar_A50_cH3(1);
  selpar_slope_cH3=set_selpar_slope_cH3(1);
  selpar_A50_cO2=set_selpar_A50_cO2(1);
  selpar_A50_cO3=set_selpar_A50_cO3(1);
  selpar_slope_cO2=set_selpar_slope_cO2(1);
  selpar_A502_cO2=set_selpar_A502_cO2(1);
  selpar_slope2_cO2=set_selpar_slope2_cO2(1);
  
  selpar_A50_HB1=set_selpar_A50_HB1(1);
  selpar_slope_HB1=set_selpar_slope_HB1(1);
  selpar_A50_HB2=set_selpar_A50_HB2(1);
  selpar_slope_HB2=set_selpar_slope_HB2(1);
  selpar_A50_HB3=set_selpar_A50_HB3(1);
  selpar_slope_HB3=set_selpar_slope_HB3(1);
  selpar_A50_GR3=set_selpar_A50_GR3(1);
  selpar_slope_GR3=set_selpar_slope_GR3(1);
  
  selpar_age1logit_D=set_selpar_age1logit_D(1);
    
  selpar_A50_CVT=set_selpar_A50_CVT(1);
  selpar_slope_CVT=set_selpar_slope_CVT(1);
  selpar_A502_CVT=set_selpar_A502_CVT(1);
  selpar_slope2_CVT=set_selpar_slope2_CVT(1);
 sqrt2pi=sqrt(2.*3.14159265);
 g2mt=0.000001;         //conversion of grams to metric tons
 g2kg=0.001;            //conversion of grams to kg 
 mt2klb=2.20462;        //conversion of metric tons to 1000 lb 
 mt2lb=mt2klb*1000.0;   //conversion of metric tons to lb
 g2klb=g2mt*mt2klb;     //conversion of grams to 1000 lb 
 dzero=0.00001;         
 huge_number=1.0e+10;   
 
 SSB_msy_out=0.0;
 iter_inc_msy=max_F_spr_msy/(n_iter_msy-1);
 iter_inc_spr=max_F_spr_msy/(n_iter_spr-1); 
 maturity_f=maturity_f_obs;
 maturity_m=maturity_m_obs;
 
 prop_f=prop_f_obs;
 prop_m=1.0-prop_f_obs;
  
      nsamp_cH_lenc_allyr=missing;
	  nsamp_cO_lenc_allyr=missing;  
	  nsamp_HB_lenc_allyr=missing;
      nsamp_HB_D_lenc_allyr=missing;  
	  nsamp_GR_lenc_allyr=missing;
	  nsamp_CVT_lenc_allyr=missing;	  
      nsamp_cH_agec_allyr=missing;
      nsamp_HB_agec_allyr=missing;
	  nsamp_GR_agec_allyr=missing;  
	  nsamp_CVT_agec_allyr=missing;  	  
      
      nfish_cH_lenc_allyr=missing;
	  nfish_cO_lenc_allyr=missing;  
	  nfish_HB_lenc_allyr=missing;
      nfish_HB_D_lenc_allyr=missing;  
	  nfish_GR_lenc_allyr=missing;
	  nfish_CVT_lenc_allyr=missing;	  
      nfish_cH_agec_allyr=missing;
      nfish_HB_agec_allyr=missing;
	  nfish_GR_agec_allyr=missing;  
	  nfish_CVT_agec_allyr=missing;  	  
   
      for (iyear=1; iyear<=nyr_cH_lenc; iyear++)
         {if (nsamp_cH_lenc(iyear)>=minSS_cH_lenc)
           {nsamp_cH_lenc_allyr(yrs_cH_lenc(iyear))=nsamp_cH_lenc(iyear);
            nfish_cH_lenc_allyr(yrs_cH_lenc(iyear))=nfish_cH_lenc(iyear);}}
      for (iyear=1; iyear<=nyr_cO_lenc; iyear++)
         {if (nsamp_cO_lenc(iyear)>=minSS_cO_lenc)
           {nsamp_cO_lenc_allyr(yrs_cO_lenc(iyear))=nsamp_cO_lenc(iyear);
            nfish_cO_lenc_allyr(yrs_cO_lenc(iyear))=nfish_cO_lenc(iyear);}}			
	  for (iyear=1; iyear<=nyr_HB_lenc; iyear++)                           
         {if (nsamp_HB_lenc(iyear)>=minSS_HB_lenc)
            {nsamp_HB_lenc_allyr(yrs_HB_lenc(iyear))=nsamp_HB_lenc(iyear);
             nfish_HB_lenc_allyr(yrs_HB_lenc(iyear))=nfish_HB_lenc(iyear);}}
      for (iyear=1; iyear<=nyr_HB_D_lenc; iyear++)                           
         {if (nsamp_HB_D_lenc(iyear)>=minSS_HB_D_lenc)
            {nsamp_HB_D_lenc_allyr(yrs_HB_D_lenc(iyear))=nsamp_HB_D_lenc(iyear);
             nfish_HB_D_lenc_allyr(yrs_HB_D_lenc(iyear))=nfish_HB_D_lenc(iyear);}}
	  for (iyear=1; iyear<=nyr_GR_lenc; iyear++)                           
         {if (nsamp_GR_lenc(iyear)>=minSS_GR_lenc)
            {nsamp_GR_lenc_allyr(yrs_GR_lenc(iyear))=nsamp_GR_lenc(iyear);
             nfish_GR_lenc_allyr(yrs_GR_lenc(iyear))=nfish_GR_lenc(iyear);}}
	  for (iyear=1; iyear<=nyr_CVT_lenc; iyear++)                           
         {if (nsamp_CVT_lenc(iyear)>=minSS_CVT_lenc)
            {nsamp_CVT_lenc_allyr(yrs_CVT_lenc(iyear))=nsamp_CVT_lenc(iyear);
             nfish_CVT_lenc_allyr(yrs_CVT_lenc(iyear))=nfish_CVT_lenc(iyear);}}
	  for (iyear=1; iyear<=nyr_cH_agec; iyear++)
         {if (nsamp_cH_agec(iyear)>=minSS_cH_agec)
           {nsamp_cH_agec_allyr(yrs_cH_agec(iyear))=nsamp_cH_agec(iyear);
            nfish_cH_agec_allyr(yrs_cH_agec(iyear))=nfish_cH_agec(iyear);}}
      for (iyear=1; iyear<=nyr_HB_agec; iyear++)
          {if (nsamp_HB_agec(iyear)>=minSS_HB_agec)
            {nsamp_HB_agec_allyr(yrs_HB_agec(iyear))=nsamp_HB_agec(iyear);
             nfish_HB_agec_allyr(yrs_HB_agec(iyear))=nfish_HB_agec(iyear);}} 
      for (iyear=1; iyear<=nyr_GR_agec; iyear++)  
         {if (nsamp_GR_agec(iyear)>=minSS_GR_agec)
           {nsamp_GR_agec_allyr(yrs_GR_agec(iyear))=nsamp_GR_agec(iyear);
             nfish_GR_agec_allyr(yrs_GR_agec(iyear))=nfish_GR_agec(iyear);}}  
	  for (iyear=1; iyear<=nyr_CVT_agec; iyear++)  
          {if (nsamp_CVT_agec(iyear)>=minSS_CVT_agec)
            {nsamp_CVT_agec_allyr(yrs_CVT_agec(iyear))=nsamp_CVT_agec(iyear);
             nfish_CVT_agec_allyr(yrs_CVT_agec(iyear))=nfish_CVT_agec(iyear);}} 
             
  F_msy(1)=0.0;  
  for (ff=2;ff<=n_iter_msy;ff++) {F_msy(ff)=F_msy(ff-1)+iter_inc_msy;}
  F_spr(1)=0.0;  
  for (ff=2;ff<=n_iter_spr;ff++) {F_spr(ff)=F_spr(ff-1)+iter_inc_spr;}
  F_cH.initialize(); L_cH_num.initialize();
  F_cO.initialize(); L_cO_num.initialize();
  F_HB.initialize(); L_HB_num.initialize();
  F_GR.initialize(); L_GR_num.initialize();
  F_cH_D.initialize(); D_cH_num.initialize();
  F_HB_D.initialize(); D_HB_num.initialize();
  F_GR_D.initialize(); D_GR_num.initialize();
  F_cH_out.initialize();
  F_cO_out.initialize();
  F_HB_out.initialize();
  F_GR_out.initialize();
  F_cH_D_out.initialize();
  F_HB_D_out.initialize();
  F_GR_D_out.initialize();
  sel_cH.initialize();
  sel_cO.initialize();  
  sel_HB.initialize();
  sel_GR.initialize();
  sel_D.initialize();
  sel_CVT.initialize();
  sel_cH_block1.initialize(); 
  sel_cH_block2.initialize();
  sel_cH_block3.initialize();  
  sel_cO_block1.initialize();
  sel_cO_block2.initialize();
  sel_cO_block3.initialize();    
  sel_HB_block1.initialize();
  sel_HB_block2.initialize();
  sel_HB_block3.initialize();
  sel_GR_block1.initialize();
  sel_GR_block2.initialize();
  sel_GR_block3.initialize();
  sel_D_block1.initialize();   
  sel_D_block2.initialize();  
  sel_D_block3.initialize();
  sel_CVT_vec.initialize();
  prob_belowsizelim_block2.initialize();
  prob_belowsizelim_block3.initialize();
  
  log_rec_dev_output.initialize();  
  log_rec_dev=set_log_rec_dev_vals;
  log_Nage_dev_output.initialize();
  log_Nage_dev=set_log_Nage_dev_vals;
 
 
}

void model_parameters::userfunction(void)
{
  fval =0.0;
 //cout<<"start"<<endl;
 //get_M_at_age(); //Needed only if M is estimated
  get_length_weight_at_age(); 
  //cout << "got length, weight, fecundity transitions" <<endl;
  get_reprod();
  //cout << "got reprod" << endl;
  get_length_at_age_dist(); 
  //cout<< "got predicted length at age distribution"<<endl;
  get_weight_at_age_landings();
  //cout<< "got weight at age of landings"<<endl; 
  get_spr_F0();
  //cout << "got F0 spr" << endl;
  get_selectivity(); 
  //cout << "got selectivity" << endl;
  get_mortality(); 
  // cout << "got mortalities" << endl;
  get_bias_corr(); 
  //cout<< "got recruitment bias correction" << endl;
  get_numbers_at_age(); 
  //cout << "got numbers at age" << endl;
  get_landings_numbers();
  //cout << "got landings in numbers" << endl;
  get_landings_wgt();
  //cout << "got landings in wgt" << endl;
  get_dead_discards(); 
  //cout << "got dead discards in num and wgt" << endl;
  get_catchability_fcns(); 
  //cout << "got catchability_fcns" << endl;
  get_indices();
  //cout << "got indices" << endl;
  get_length_comps();
  // cout<< "got length comps"<< endl;
  get_age_comps();
  //cout<< "got age comps"<< endl;
   evaluate_objective_function();
   //cout << "objective function calculations complete" << endl;
}

void model_parameters::get_length_weight_at_age(void)
{
	//population total length in mm
    //compute mean length (mm TL) and weight (whole) at age
    meanlen_TL=Linf*(1.0-mfexp(-K*(agebins-t0+0.5)));     
    wgt_kg=wgtpar_a*pow(meanlen_TL,wgtpar_b);             //whole wgt in kg 
    wgt_g=wgt_kg/g2kg;                                    //convert wgt in kg to weight in g    
    wgt_mt=wgt_g*g2mt;                                    //convert weight in g to weight in mt
    wgt_klb=mt2klb*wgt_mt;                                //1000 lb of whole wgt
    wgt_lb=mt2lb*wgt_mt;                                  //lb of whole wgt
}

void model_parameters::get_reprod(void)
{
    reprod=elem_prod((elem_prod(prop_f,maturity_f)+elem_prod(prop_m,maturity_m)),wgt_mt);
}

void model_parameters::get_length_at_age_dist(void)
{
  //compute matrix of length at age, based on the normal distribution
    //population
	for (iage=1;iage<=nages;iage++)
   {len_cv(iage)=len_cv_val;
    len_sd(iage)=meanlen_TL(iage)*len_cv(iage);
	zscore_lzero=(0.0-meanlen_TL(iage))/len_sd(iage); 
	cprob_lzero=cumd_norm(zscore_lzero);
    //first length bin
	//population
    zscore_len=((lenbins(1)+0.5*lenbins_width)-meanlen_TL(iage)) / len_sd(iage);
    cprob_lenvec(1)=cumd_norm(zscore_len);          //includes any probability mass below zero
    lenprob(iage,1)=cprob_lenvec(1)-cprob_lzero;    //removes any probability mass below zero
	zscore_lsizelim1=(sizelim1-meanlen_TL(iage)) / len_sd(iage);
	cprob_lsizelim1=cumd_norm(zscore_lsizelim1);                 //includes any probability mass below zero
	prob_belowsizelim_block2(iage)=	cprob_lsizelim1-cprob_lzero; //removes any probability mass below zero
	zscore_lsizelim2=(sizelim2-meanlen_TL(iage)) / len_sd(iage);
	cprob_lsizelim2=cumd_norm(zscore_lsizelim2);                 //includes any probability mass below zero
	prob_belowsizelim_block3(iage)=	cprob_lsizelim2-cprob_lzero; //removes any probability mass below zero
    //most other length bins  
    //population
    for (ilen=2;ilen<nlenbins;ilen++)
      {
        zscore_len=((lenbins(ilen)+0.5*lenbins_width)-meanlen_TL(iage)) / len_sd(iage); 
		cprob_lenvec(ilen)=cumd_norm(zscore_len);
        lenprob(iage,ilen)=cprob_lenvec(ilen)-cprob_lenvec(ilen-1);
      }
    //last length bin is a plus group
	//population
    zscore_len=((lenbins(nlenbins)-0.5*lenbins_width)-meanlen_TL(iage)) / len_sd(iage); 
	lenprob(iage,nlenbins)=1.0-cumd_norm(zscore_len);
      lenprob(iage)=lenprob(iage)/(1.0-cprob_lzero);  //renormalize to account for any prob mass below size=0
   }
  //fleet and survey specific length probs, all assumed here to equal the popn
  lenprob_cH=lenprob;
  lenprob_cO=lenprob; 
  lenprob_HB=lenprob;
  lenprob_HB_D=lenprob; 
  lenprob_GR=lenprob;  
  lenprob_CVT=lenprob; 
}

void model_parameters::get_weight_at_age_landings(void)
{
  for (iyear=styr; iyear<=endyr; iyear++)
  {
    len_cH_mm(iyear)=meanlen_TL;  
    wholewgt_cH_klb(iyear)=wgt_klb; 
    len_cO_mm(iyear)=meanlen_TL;  
    wholewgt_cO_klb(iyear)=wgt_klb; 	
    len_HB_mm(iyear)=meanlen_TL;
    wholewgt_HB_klb(iyear)=wgt_klb;
    len_GR_mm(iyear)=meanlen_TL;
    wholewgt_GR_klb(iyear)=wgt_klb;
    len_cH_D_mm(iyear)=meanlen_TL;  
    wholewgt_cH_D_klb(iyear)=wgt_klb;
    len_HB_D_mm(iyear)=meanlen_TL;
    wholewgt_HB_D_klb(iyear)=wgt_klb;
    len_GR_D_mm(iyear)=meanlen_TL;
    wholewgt_GR_D_klb(iyear)=wgt_klb;
  }  
}

void model_parameters::get_spr_F0(void)
{
  //at mdyr, apply half this yr's mortality, half next yr's
  N_spr_F0(1)=1.0*mfexp(-1.0*M(1)*spawn_time_frac); //at peak spawning time
  N_bpr_F0(1)=1.0;      //at start of year
  for (iage=2; iage<=nages; iage++)
  { N_spr_F0(iage)=N_spr_F0(iage-1)*mfexp(-1.0*(M(iage-1)*(1.0-spawn_time_frac) + M(iage)*spawn_time_frac)); 
    N_bpr_F0(iage)=N_bpr_F0(iage-1)*mfexp(-1.0*(M(iage-1)));    
  }
  N_spr_F0(nages)=N_spr_F0(nages)/(1.0-mfexp(-1.0*M(nages))); //plus group (sum of geometric series)
  N_bpr_F0(nages)=N_bpr_F0(nages)/(1.0-mfexp(-1.0*M(nages)));
  spr_F0=sum(elem_prod(N_spr_F0,reprod)); 
  bpr_F0=sum(elem_prod(N_bpr_F0,wgt_mt));    
}

void model_parameters::get_selectivity(void)
{
  sel_cH_block2=logistic(agebins, selpar_A50_cH2, selpar_slope_cH2);
  sel_cH_block3=logistic(agebins, selpar_A50_cH3, selpar_slope_cH3);
  sel_cH_block1=sel_cH_block2;
  sel_cO_block2=logistic_double(agebins, selpar_A50_cO2, selpar_slope_cO2, selpar_A502_cO2,selpar_slope2_cO2);
  sel_cO_block3=logistic_double(agebins, selpar_A50_cO3, selpar_slope_cO2, selpar_A502_cO2,selpar_slope2_cO2);
  sel_cO_block1=sel_cO_block2;
  sel_HB_block1=logistic(agebins, selpar_A50_HB1, selpar_slope_HB1);
  sel_HB_block2=logistic(agebins, selpar_A50_HB2, selpar_slope_HB2);
  sel_HB_block3=logistic(agebins, selpar_A50_HB3, selpar_slope_HB3);
  sel_GR_block1=sel_HB_block1;
  sel_GR_block2=sel_HB_block2;
  sel_GR_block3=logistic(agebins, selpar_A50_GR3, selpar_slope_GR3);
  sel_CVT_vec=logistic_double(agebins, selpar_A50_CVT, selpar_slope_CVT, selpar_A502_CVT,selpar_slope2_CVT);
  selpar_age1_D=mfexp(selpar_age1logit_D)/(1.0+mfexp(selpar_age1logit_D));
  sel_D_block2(1)=selpar_age1_D; sel_D_block2(2)=1.0; sel_D_block2(3,nages)=prob_belowsizelim_block2(3,nages);
  sel_D_block3(1)=selpar_age1_D; sel_D_block3(2)=1.0; sel_D_block3(3,nages)=prob_belowsizelim_block3(3,nages);
  sel_D_block1=sel_D_block2;
  //BLOCK 1 for selex. No size limit   
  for (iyear=styr; iyear<=endyr_selex_phase1; iyear++)
   {     
    sel_cH(iyear)=sel_cH_block1;
	sel_cO(iyear)=sel_cO_block1;
    sel_HB(iyear)=sel_HB_block1;
    sel_GR(iyear)=sel_GR_block1;
	sel_CVT(iyear)=sel_CVT_vec;
 	sel_D(iyear)=sel_D_block1;
   }
  //BLOCK 2 for selex. 12" size limit
  for (iyear=(endyr_selex_phase1+1); iyear<=endyr_selex_phase2; iyear++)
   {
    sel_cH(iyear)=sel_cH_block2;
	sel_cO(iyear)=sel_cO_block2;
    sel_HB(iyear)=sel_HB_block2;
    sel_GR(iyear)=sel_GR_block2;
	sel_CVT(iyear)=sel_CVT_vec;
 	sel_D(iyear)=sel_D_block2;
   }
  //BLOCK 3 for selex.  20" size limit
   for (iyear=(endyr_selex_phase2+1); iyear<=endyr; iyear++)
   {   
    sel_cH(iyear)=sel_cH_block3;
	sel_cO(iyear)=sel_cO_block3;
    sel_HB(iyear)=sel_HB_block3;
    sel_GR(iyear)=sel_GR_block3;
	sel_CVT(iyear)=sel_CVT_vec;
 	sel_D(iyear)=sel_D_block3;
   }  
}

void model_parameters::get_mortality(void)
{
  Fsum.initialize();
  Fapex.initialize();
  F.initialize();
  //initialization F is avg from first 3 yrs of observed landings
  log_F_dev_init_cH=sum(log_F_dev_cH(styr_cH_L,(styr_cH_L+2)))/3.0;  
  log_F_dev_init_cO=sum(log_F_dev_cO(styr_cO_L,(styr_cO_L+2)))/3.0;  
  log_F_dev_init_HB=sum(log_F_dev_HB(styr_HB_L,(styr_HB_L+2)))/3.0;         
  log_F_dev_init_GR=sum(log_F_dev_GR(styr_GR_L,(styr_GR_L+2)))/3.0;         
  for (iyear=styr; iyear<=endyr; iyear++) 
  {
    if(iyear>=styr_cH_L & iyear<=endyr_cH_L) //spans full time series
		{F_cH_out(iyear)=mfexp(log_avg_F_cH+log_F_dev_cH(iyear));}     
    F_cH(iyear)=sel_cH(iyear)*F_cH_out(iyear);
    Fsum(iyear)+=F_cH_out(iyear);
	if(iyear>=styr_cO_L & iyear<=endyr_cO_L) //spans full time series
		{F_cO_out(iyear)=mfexp(log_avg_F_cO+log_F_dev_cO(iyear));}     
    F_cO(iyear)=sel_cO(iyear)*F_cO_out(iyear);
    Fsum(iyear)+=F_cO_out(iyear);
    if(iyear>=styr_HB_L & iyear<=endyr_HB_L) //spans full time series
		{F_HB_out(iyear)=mfexp(log_avg_F_HB+log_F_dev_HB(iyear));}     
    F_HB(iyear)=sel_HB(iyear)*F_HB_out(iyear);
    Fsum(iyear)+=F_HB_out(iyear);
    if(iyear>=styr_GR_L & iyear<=endyr_GR_L) //starts in 1981
		{F_GR_out(iyear)=mfexp(log_avg_F_GR+log_F_dev_GR(iyear));}    
    if (iyear<styr_GR_L)
		{F_GR_out(iyear)=mfexp(log_avg_F_GR+log_F_dev_init_GR);}
	F_GR(iyear)=sel_GR(iyear)*F_GR_out(iyear); 
    Fsum(iyear)+=F_GR_out(iyear);
    log_F_avgdev_cH_D=sum(log_F_dev_cH_D(styr_cH_D,endyr_cH_D))/(endyr_cH_D-styr_cH_D+1.0);
    log_F_avgdev_HB_D=sum(log_F_dev_HB_D(styr_HB_D,endyr_HB_D))/(endyr_HB_D-styr_HB_D+1.0);   
    log_F_avgdev_GR_D=sum(log_F_dev_GR_D(styr_GR_D,endyr_GR_D))/(endyr_GR_D-styr_GR_D+1.0);	
    if(iyear>=styr_cH_D & iyear<=endyr_cH_D)
		{F_cH_D_out(iyear)=mfexp(log_avg_F_cH_D+log_F_dev_cH_D(iyear));}    
    if(iyear>endyr_selex_phase1 & iyear<styr_cH_D)
		{F_cH_D_out(iyear)=mfexp(log_avg_F_cH_D+log_F_avgdev_cH_D);} 	
    F_cH_D(iyear)=sel_D(iyear)*F_cH_D_out(iyear);
    Fsum(iyear)+=F_cH_D_out(iyear);
    if(iyear>=styr_HB_D & iyear<=endyr_HB_D)
		{F_HB_D_out(iyear)=mfexp(log_avg_F_HB_D+log_F_dev_HB_D(iyear));}    
    if(iyear>endyr_selex_phase1 & iyear<styr_HB_D)
      {F_HB_D_out(iyear)=mfexp(log_avg_F_HB_D+log_F_avgdev_HB_D);} 	
    F_HB_D(iyear)=sel_D(iyear)*F_HB_D_out(iyear);
    Fsum(iyear)+=F_HB_D_out(iyear);
    if(iyear>=styr_GR_D & iyear<=endyr_GR_D)
		{F_GR_D_out(iyear)=mfexp(log_avg_F_GR_D+log_F_dev_GR_D(iyear));}  
	F_GR_D(iyear)=sel_D(iyear)*F_GR_D_out(iyear); 
	Fsum(iyear)+=F_GR_D_out(iyear);
    //Total F at age
    F(iyear)=F_cH(iyear);  //first in additive series (NO +=)
    F(iyear)+=F_cO(iyear);
    F(iyear)+=F_HB(iyear);
    F(iyear)+=F_GR(iyear);
    F(iyear)+=F_cH_D(iyear);
    F(iyear)+=F_HB_D(iyear);
    F(iyear)+=F_GR_D(iyear);
    Fapex(iyear)=max(F(iyear));
    Z(iyear)=M+F(iyear);
   }  //end iyear 
}

void model_parameters::get_bias_corr(void)
{
  var_rec_dev=norm2(log_rec_dev(styr_rec_dev,endyr_rec_dev)-
              sum(log_rec_dev(styr_rec_dev,endyr_rec_dev))/nyrs_rec)
              /(nyrs_rec-1.0);                           
  //if (set_BiasCor <= 0.0) {BiasCor=mfexp(var_rec_dev/2.0);}   //bias correction based on empirical residuals
  rec_sigma_sq=square(rec_sigma);
  if (set_BiasCor <= 0.0) {BiasCor=mfexp(rec_sigma_sq/2.0);}   //bias correction based on Rsigma               
  else {BiasCor=set_BiasCor;}
}

void model_parameters::get_numbers_at_age(void)
{
  R0=mfexp(log_R0);
  S0=spr_F0*R0;
  R_virgin=SR_eq_func(R0, steep, spr_F0, spr_F0, BiasCor, SR_switch);
  B0=bpr_F0*R_virgin;   
  B0_q_DD=R_virgin*sum(elem_prod(N_bpr_F0(set_q_DD_stage,nages),wgt_mt(set_q_DD_stage,nages))); 
  F_initial=sel_cH(styr)*mfexp(log_avg_F_cH+log_F_dev_init_cH)+
            sel_cO(styr)*mfexp(log_avg_F_cO+log_F_dev_init_cO)+
            sel_HB(styr)*mfexp(log_avg_F_HB+log_F_dev_init_HB)+
            sel_GR(styr)*mfexp(log_avg_F_GR+log_F_dev_init_GR);			
  Z_initial=M+F_initial;
  N_spr_initial(1)=1.0*mfexp(-1.0*Z_initial(1)*spawn_time_frac); //at peak spawning time;
  for (iage=2; iage<=nages; iage++)
    {
      N_spr_initial(iage)=N_spr_initial(iage-1)*
                   mfexp(-1.0*(Z_initial(iage-1)*(1.0-spawn_time_frac) + Z_initial(iage)*spawn_time_frac)); 
    }
  N_spr_initial(nages)=N_spr_initial(nages)/(1.0-mfexp(-1.0*Z_initial(nages))); //plus group
  spr_initial=sum(elem_prod(N_spr_initial,reprod));
  if (styr==styr_rec_dev) {R1=SR_eq_func(R0, steep, spr_F0, spr_initial, 1.0, SR_switch);} //without bias correction (deviation added later)
  else {R1=SR_eq_func(R0, steep, spr_F0, spr_initial, BiasCor, SR_switch);} //with bias correction
  if(R1<10.0) {R1=10.0;} //Avoid unrealistically low popn sizes during search algorithm
  N_initial_eq(1)=R1;
  for (iage=2; iage<=nages; iage++)
  {
    N_initial_eq(iage)=N_initial_eq(iage-1)*
        mfexp(-1.0*(Z_initial(iage-1)));    
  }
  //plus group calculation
  N_initial_eq(nages)=N_initial_eq(nages)/(1.0-mfexp(-1.0*Z_initial(nages))); //plus group
  N(styr)(2,nages)=elem_prod(N_initial_eq(2,nages),mfexp(log_Nage_dev));
  if (styr==styr_rec_dev) {N(styr,1)=N_initial_eq(1)*mfexp(log_rec_dev(styr_rec_dev));}
  else {N(styr,1)=N_initial_eq(1);}
  N_mdyr(styr)(1,nages)=elem_prod(N(styr)(1,nages),(mfexp(-1.*(Z_initial(1,nages))*0.5))); //mid year 
  N_spawn(styr)(1,nages)=elem_prod(N(styr)(1,nages),(mfexp(-1.*(Z_initial(1,nages))*spawn_time_frac))); //peak spawning time 
  SSB(styr)=sum(elem_prod(N_spawn(styr),reprod));
  B_q_DD(styr)=sum(elem_prod(N(styr)(set_q_DD_stage,nages),wgt_mt(set_q_DD_stage,nages)));
  for (iyear=styr; iyear<endyr; iyear++)
  {
    if(iyear<(styr_rec_dev-1)||iyear>(endyr_rec_dev-1)) //recruitment follows S-R curve (with bias correction) exactly
    {
        N(iyear+1,1)=BiasCor*SR_func(R0, steep, spr_F0, SSB(iyear),SR_switch);
        N(iyear+1)(2,nages)=++elem_prod(N(iyear)(1,nages-1),(mfexp(-1.*Z(iyear)(1,nages-1))));
        N(iyear+1,nages)+=N(iyear,nages)*mfexp(-1.*Z(iyear,nages)); //plus group
        N_mdyr(iyear+1)(1,nages)=elem_prod(N(iyear+1)(1,nages),(mfexp(-1.*(Z(iyear+1)(1,nages))*0.5))); //mid year 
        N_spawn(iyear+1)(1,nages)=elem_prod(N(iyear+1)(1,nages),(mfexp(-1.*(Z(iyear+1)(1,nages))*spawn_time_frac))); //peak spawning time 
        SSB(iyear+1)=sum(elem_prod(N_spawn(iyear+1),reprod));
		B_q_DD(iyear+1)=sum(elem_prod(N(iyear+1)(set_q_DD_stage,nages),wgt_mt(set_q_DD_stage,nages)));       
    }
    else   //recruitment follows S-R curve with lognormal deviation
    {
        N(iyear+1,1)=SR_func(R0, steep, spr_F0, SSB(iyear),SR_switch)*mfexp(log_rec_dev(iyear+1));
        N(iyear+1)(2,nages)=++elem_prod(N(iyear)(1,nages-1),(mfexp(-1.*Z(iyear)(1,nages-1))));
        N(iyear+1,nages)+=N(iyear,nages)*mfexp(-1.*Z(iyear,nages)); //plus group
        N_mdyr(iyear+1)(1,nages)=elem_prod(N(iyear+1)(1,nages),(mfexp(-1.*(Z(iyear+1)(1,nages))*0.5))); //mid year 
        N_spawn(iyear+1)(1,nages)=elem_prod(N(iyear+1)(1,nages),(mfexp(-1.*(Z(iyear+1)(1,nages))*spawn_time_frac))); //peak spawning time 
        SSB(iyear+1)=sum(elem_prod(N_spawn(iyear+1),reprod));
        B_q_DD(iyear+1)=sum(elem_prod(N(iyear+1)(set_q_DD_stage,nages),wgt_mt(set_q_DD_stage,nages)));
    }
  }
  //last year (projection) has no recruitment variability
  N(endyr+1,1)=BiasCor*SR_func(R0, steep, spr_F0, SSB(endyr),SR_switch);
  N(endyr+1)(2,nages)=++elem_prod(N(endyr)(1,nages-1),(mfexp(-1.*Z(endyr)(1,nages-1))));
  N(endyr+1,nages)+=N(endyr,nages)*mfexp(-1.*Z(endyr,nages)); //plus group
}

void model_parameters::get_landings_numbers(void)
{
  for (iyear=styr; iyear<=endyr; iyear++)
  {
    for (iage=1; iage<=nages; iage++)
    {
      L_cH_num(iyear,iage)=N(iyear,iage)*F_cH(iyear,iage)*
        (1.-mfexp(-1.*Z(iyear,iage)))/Z(iyear,iage);
      L_cO_num(iyear,iage)=N(iyear,iage)*F_cO(iyear,iage)*
        (1.-mfexp(-1.*Z(iyear,iage)))/Z(iyear,iage);		
      L_HB_num(iyear,iage)=N(iyear,iage)*F_HB(iyear,iage)*
        (1.-mfexp(-1.*Z(iyear,iage)))/Z(iyear,iage);
      L_GR_num(iyear,iage)=N(iyear,iage)*F_GR(iyear,iage)*
        (1.-mfexp(-1.*Z(iyear,iage)))/Z(iyear,iage);        
    }          
    pred_cH_L_knum(iyear)=sum(L_cH_num(iyear))/1000.0;
	pred_cO_L_knum(iyear)=sum(L_cO_num(iyear))/1000.0;
    pred_HB_L_knum(iyear)=sum(L_HB_num(iyear))/1000.0;
    pred_GR_L_knum(iyear)=sum(L_GR_num(iyear))/1000.0;
  }
}

void model_parameters::get_landings_wgt(void)
{
  for (iyear=styr; iyear<=endyr; iyear++)
  {    
    L_cH_klb(iyear)=elem_prod(L_cH_num(iyear),wholewgt_cH_klb(iyear));     //in 1000 lb whole weight
	L_cO_klb(iyear)=elem_prod(L_cO_num(iyear),wholewgt_cO_klb(iyear));     //in 1000 lb whole weight
    L_HB_klb(iyear)=elem_prod(L_HB_num(iyear),wholewgt_HB_klb(iyear));     //in 1000 lb whole weight
    L_GR_klb(iyear)=elem_prod(L_GR_num(iyear),wholewgt_GR_klb(iyear));     //in 1000 lb whole weight
    pred_cH_L_klb(iyear)=sum(L_cH_klb(iyear));
	pred_cO_L_klb(iyear)=sum(L_cO_klb(iyear));
    pred_HB_L_klb(iyear)=sum(L_HB_klb(iyear));
    pred_GR_L_klb(iyear)=sum(L_GR_klb(iyear));    
  }
}

void model_parameters::get_dead_discards(void)
{
  //dead discards at age (number fish) 
  //for (iyear=styr_cH_D; iyear<=endyr_cH_D; iyear++)
  for (iyear=styr; iyear<=endyr; iyear++)	  
  {
    for (iage=1; iage<=nages; iage++)
    {
      D_cH_num(iyear,iage)=N(iyear,iage)*F_cH_D(iyear,iage)*
        (1.-mfexp(-1.*Z(iyear,iage)))/Z(iyear,iage);
    }
    pred_cH_D_knum(iyear)=sum(D_cH_num(iyear))/1000.0;            //pred annual dead discards in 1000s (for matching data)
    pred_cH_D_klb(iyear)=sum(elem_prod(D_cH_num(iyear),wholewgt_cH_D_klb(iyear))); //annual dead discards in 1000 lb whole (for output only)
  }
  //for (iyear=styr_HB_D; iyear<=endyr_HB_D; iyear++)
  for (iyear=styr; iyear<=endyr; iyear++)
  {
    for (iage=1; iage<=nages; iage++)
    {
      D_HB_num(iyear,iage)=N(iyear,iage)*F_HB_D(iyear,iage)*
        (1.-mfexp(-1.*Z(iyear,iage)))/Z(iyear,iage);
    }
    pred_HB_D_knum(iyear)=sum(D_HB_num(iyear))/1000.0;            //pred annual dead discards in 1000s (for matHBing data)
    pred_HB_D_klb(iyear)=sum(elem_prod(D_HB_num(iyear),wholewgt_HB_D_klb(iyear))); //annual dead discards in 1000 lb whole (for output only)
  }
  //for (iyear=styr_GR_D; iyear<=endyr_GR_D; iyear++)
  for (iyear=styr; iyear<=endyr; iyear++)
  {
    for (iage=1; iage<=nages; iage++)
    {
      D_GR_num(iyear,iage)=N(iyear,iage)*F_GR_D(iyear,iage)*
        (1.-mfexp(-1.*Z(iyear,iage)))/Z(iyear,iage);
    }
    pred_GR_D_knum(iyear)=sum(D_GR_num(iyear))/1000.0;            //pred annual dead discards in 1000s (for matGRing data)
    pred_GR_D_klb(iyear)=sum(elem_prod(D_GR_num(iyear),wholewgt_GR_D_klb(iyear))); //annual dead discards in 1000 lb whole (for output only)
  }
}

void model_parameters::get_catchability_fcns(void)
{
 //Get rate increase if estimated, otherwise fixed above
  if (set_q_rate_phase>0.0)
  {
      for (iyear=styr_cH_cpue; iyear<=endyr_cH_cpue; iyear++)
      {   if (iyear>styr_cH_cpue & iyear <=2003) 
          {//q_rate_fcn_cH(iyear)=(1.0+q_rate)*q_rate_fcn_cH(iyear-1); //compound
             q_rate_fcn_cH(iyear)=(1.0+(iyear-styr_cH_cpue)*q_rate)*q_rate_fcn_cH(styr_cH_cpue);  //linear
          }
          if (iyear>2003) {q_rate_fcn_cH(iyear)=q_rate_fcn_cH(iyear-1);} 
      }   
      for (iyear=styr_HB_cpue; iyear<=endyr_HB_cpue; iyear++)
      {   if (iyear>styr_HB_cpue & iyear <=2003) 
          {//q_rate_fcn_HB(iyear)=(1.0+q_rate)*q_rate_fcn_HB(iyear-1); //compound
             q_rate_fcn_HB(iyear)=(1.0+(iyear-styr_HB_cpue)*q_rate)*q_rate_fcn_HB(styr_HB_cpue);  //linear
          }
          if (iyear>2003) {q_rate_fcn_HB(iyear)=q_rate_fcn_HB(iyear-1);} 
      }   
      // for (iyear=styr_GR_cpue; iyear<=endyr_GR_cpue; iyear++)
      // {   if (iyear>styr_GR_cpue & iyear <=2003) 
          // {//q_rate_fcn_GR(iyear)=(1.0+q_rate)*q_rate_fcn_GR(iyear-1); //compound
             // q_rate_fcn_GR(iyear)=(1.0+(iyear-styr_GR_cpue)*q_rate)*q_rate_fcn_GR(styr_GR_cpue);  //linear
          // }
          // if (iyear>2003) {q_rate_fcn_GR(iyear)=q_rate_fcn_GR(iyear-1);} 
      // }   
  } //end q_rate conditional      
 //Get density dependence scalar (=1.0 if density independent model is used)   
  if (q_DD_beta>0.0) 
  {
    B_q_DD+=dzero;
    for (iyear=styr;iyear<=endyr;iyear++)
        {q_DD_fcn(iyear)=pow(B0_q_DD,q_DD_beta)*pow(B_q_DD(iyear),-q_DD_beta);}
          //{q_DD_fcn(iyear)=1.0+4.0/(1.0+mfexp(0.75*(B_q_DD(iyear)-0.1*B0_q_DD))); }
  }  
}

void model_parameters::get_indices(void)
{
 //cH  cpue
  q_cH(styr_cH_cpue)=mfexp(log_q_cH); 
  for (iyear=styr_cH_cpue; iyear<=endyr_cH_cpue; iyear++)
  {//index in weight units. original index in lb and re-scaled. predicted in klb whole weight, but difference in lb and klb is absorbed by q
      N_cH(iyear)=elem_prod(elem_prod(N_mdyr(iyear),sel_cH(iyear)),wholewgt_cH_klb(iyear));   
      pred_cH_cpue(iyear)=q_cH(iyear)*q_rate_fcn_cH(iyear)*q_DD_fcn(iyear)*sum(N_cH(iyear));
      if (iyear<endyr_cH_cpue){q_cH(iyear+1)=q_cH(iyear)*mfexp(q_RW_log_dev_cH(iyear));}
  }
 //HB  cpue
  q_HB(styr_HB_cpue)=mfexp(log_q_HB); 
  for (iyear=styr_HB_cpue; iyear<=endyr_HB_cpue; iyear++)
  {   
      N_HB(iyear)=elem_prod(N_mdyr(iyear),sel_HB(iyear)); 
      pred_HB_cpue(iyear)=q_HB(iyear)*q_rate_fcn_HB(iyear)*q_DD_fcn(iyear)*sum(N_HB(iyear));
      if (iyear<endyr_HB_cpue){q_HB(iyear+1)=q_HB(iyear)*mfexp(q_RW_log_dev_HB(iyear));}
  }
 // //GR cpue 
  // sel_GR_cpue.initialize(); //approximate selectivity; includes discards (live + dead)
  // q_GR(styr_GR_cpue)=mfexp(log_q_GR); 
  // for (iyear=styr_GR_cpue; iyear<=endyr_GR_cpue; iyear++)
  // {   
      // sel_GR_cpue(iyear)=F_GR(iyear)+F_GR_D(iyear)/Dmort_GR;
      // sel_GR_cpue(iyear)/=max(sel_GR_cpue(iyear));
      // N_GR(iyear)=elem_prod(N_mdyr(iyear),sel_GR_cpue(iyear)); 
      // pred_GR_cpue(iyear)=q_GR(iyear)*q_rate_fcn_GR(iyear)*q_DD_fcn(iyear)*sum(N_GR(iyear));
      // if (iyear<endyr_GR_cpue){q_GR(iyear+1)=q_GR(iyear)*mfexp(q_RW_log_dev_GR(iyear));}
  // } 
 //SEFIS CVT cpue
  q_CVT(styr_CVT_cpue)=mfexp(log_q_CVT); 
  for (iyear=styr_CVT_cpue; iyear<=endyr_CVT_cpue; iyear++)
  {   
    N_CVT(iyear)=elem_prod(N_mdyr(iyear),sel_CVT(iyear)); 
	pred_CVT_cpue(iyear)=q_CVT(iyear)*q_DD_fcn(iyear)*sum(N_CVT(iyear));
    if (iyear<endyr_CVT_cpue){q_CVT(iyear+1)=q_CVT(iyear)*mfexp(q_RW_log_dev_CVT(iyear));}
  } 
}

void model_parameters::get_length_comps(void)
{
  //comm lines 
  for (iyear=1;iyear<=nyr_cH_lenc;iyear++)
  {pred_cH_lenc(iyear)=(L_cH_num(yrs_cH_lenc(iyear))*lenprob_cH)/sum(L_cH_num(yrs_cH_lenc(iyear)));}
  //comm other 
  for (iyear=1;iyear<=nyr_cO_lenc;iyear++)
  {pred_cO_lenc(iyear)=(L_cO_num(yrs_cO_lenc(iyear))*lenprob_cO)/sum(L_cO_num(yrs_cO_lenc(iyear)));}
  //headboat    
  for (iyear=1;iyear<=nyr_HB_lenc;iyear++) 
  {pred_HB_lenc(iyear)=(L_HB_num(yrs_HB_lenc(iyear))*lenprob_HB)/sum(L_HB_num(yrs_HB_lenc(iyear)));}
  //headboat discards   
  for (iyear=1;iyear<=nyr_HB_D_lenc;iyear++) 
  {pred_HB_D_lenc(iyear)=(D_HB_num(yrs_HB_D_lenc(iyear))*lenprob_HB_D)/sum(D_HB_num(yrs_HB_D_lenc(iyear)));}
 //general rec   
  for (iyear=1;iyear<=nyr_GR_lenc;iyear++) 
  {pred_GR_lenc(iyear)=(L_GR_num(yrs_GR_lenc(iyear))*lenprob_GR)/sum(L_GR_num(yrs_GR_lenc(iyear)));}
  //CVT    
  for (iyear=1;iyear<=nyr_CVT_lenc;iyear++) 
  {pred_CVT_lenc(iyear)=(N_CVT(yrs_CVT_lenc(iyear))*lenprob_CVT)/sum(N_CVT(yrs_CVT_lenc(iyear)));}
}

void model_parameters::get_age_comps(void)
{
  //Commercial handline
  for (iyear=1;iyear<=nyr_cH_agec;iyear++) 
  {
    ErrorFree_cH_agec(iyear)=L_cH_num(yrs_cH_agec(iyear))/sum(L_cH_num(yrs_cH_agec(iyear)));  
    pred_cH_agec_allages(iyear)=age_error*(ErrorFree_cH_agec(iyear)/sum(ErrorFree_cH_agec(iyear)));   
    for (iage=1; iage<=nages_agec; iage++) {pred_cH_agec(iyear,iage)=pred_cH_agec_allages(iyear,iage);} 
    //for (iage=(nages_agec+1); iage<=nages; iage++) {pred_cH_agec(iyear,nages_agec)+=pred_cH_agec_allages(iyear,iage);} //plus group                             
  }
  //Headboat
 for (iyear=1;iyear<=nyr_HB_agec;iyear++)
  {
    ErrorFree_HB_agec(iyear)=L_HB_num(yrs_HB_agec(iyear))/sum(L_HB_num(yrs_HB_agec(iyear)));
    pred_HB_agec_allages(iyear)=age_error*ErrorFree_HB_agec(iyear); 
    for (iage=1; iage<=nages_agec; iage++) {pred_HB_agec(iyear,iage)=pred_HB_agec_allages(iyear,iage);} 
    //for (iage=(nages_agec+1); iage<=nages; iage++) {pred_HB_agec(iyear,nages_agec_HB)+=pred_HB_agec_allages(iyear,iage);} //plus group                        
  }
   //Recreational 
 for (iyear=1;iyear<=nyr_GR_agec;iyear++)
  {
    ErrorFree_GR_agec(iyear)=L_GR_num(yrs_GR_agec(iyear))/sum(L_GR_num(yrs_GR_agec(iyear)));
    pred_GR_agec_allages(iyear)=age_error*ErrorFree_GR_agec(iyear); 
    for (iage=1; iage<=nages_agec; iage++) {pred_GR_agec(iyear,iage)=pred_GR_agec_allages(iyear,iage);} 
    //for (iage=(nages_agec+1); iage<=nages; iage++) {pred_GR_agec(iyear,nages_agec)+=pred_GR_agec_allages(iyear,iage);} //plus group                        
  }
   //MARMAP CVT 
 for (iyear=1;iyear<=nyr_CVT_agec;iyear++)
  {
    ErrorFree_CVT_agec(iyear)=N_CVT(yrs_CVT_agec(iyear))/sum(N_CVT(yrs_CVT_agec(iyear)));
    pred_CVT_agec_allages(iyear)=age_error*ErrorFree_CVT_agec(iyear); 
    for (iage=1; iage<=nages_agec; iage++) {pred_CVT_agec(iyear,iage)=pred_CVT_agec_allages(iyear,iage);} 
    //for (iage=(nages_agec+1); iage<=nages; iage++) {pred_CVT_agec(iyear,nages_agec)+=pred_CVT_agec_allages(iyear,iage);} //plus group                        
  }
}

void model_parameters::get_weighted_current(void)
{
  F_temp_sum=0.0;
  F_temp_sum+=mfexp((selpar_n_yrs_wgted*log_avg_F_cH+
        sum(log_F_dev_cH((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted);  
  F_temp_sum+=mfexp((selpar_n_yrs_wgted*log_avg_F_cO+
        sum(log_F_dev_cO((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted);  
  F_temp_sum+=mfexp((selpar_n_yrs_wgted*log_avg_F_HB+
        sum(log_F_dev_HB((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted);
  F_temp_sum+=mfexp((selpar_n_yrs_wgted*log_avg_F_GR+
        sum(log_F_dev_GR((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted);
  F_temp_sum+=mfexp((selpar_n_yrs_wgted*log_avg_F_cH_D+
        sum(log_F_dev_cH_D((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted);
  F_temp_sum+=mfexp((selpar_n_yrs_wgted*log_avg_F_HB_D+
        sum(log_F_dev_HB_D((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted);
  F_temp_sum+=mfexp((selpar_n_yrs_wgted*log_avg_F_GR_D+
        sum(log_F_dev_GR_D((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted);
  F_cH_prop=mfexp((selpar_n_yrs_wgted*log_avg_F_cH+
        sum(log_F_dev_cH((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted)/F_temp_sum;
  F_cO_prop=mfexp((selpar_n_yrs_wgted*log_avg_F_cO+
        sum(log_F_dev_cO((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted)/F_temp_sum;
  F_HB_prop=mfexp((selpar_n_yrs_wgted*log_avg_F_HB+
        sum(log_F_dev_HB((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted)/F_temp_sum;
  F_GR_prop=mfexp((selpar_n_yrs_wgted*log_avg_F_GR+
        sum(log_F_dev_GR((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted)/F_temp_sum;
  F_cH_D_prop=mfexp((selpar_n_yrs_wgted*log_avg_F_cH_D+
        sum(log_F_dev_cH_D((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted)/F_temp_sum;
  F_HB_D_prop=mfexp((selpar_n_yrs_wgted*log_avg_F_HB_D+
        sum(log_F_dev_HB_D((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted)/F_temp_sum;
  F_GR_D_prop=mfexp((selpar_n_yrs_wgted*log_avg_F_GR_D+
        sum(log_F_dev_GR_D((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted)/F_temp_sum;
  log_F_dev_end_cH=sum(log_F_dev_cH((endyr-selpar_n_yrs_wgted+1),endyr))/selpar_n_yrs_wgted;
  log_F_dev_end_cO=sum(log_F_dev_cO((endyr-selpar_n_yrs_wgted+1),endyr))/selpar_n_yrs_wgted;
  log_F_dev_end_HB=sum(log_F_dev_HB((endyr-selpar_n_yrs_wgted+1),endyr))/selpar_n_yrs_wgted;  
  log_F_dev_end_GR=sum(log_F_dev_GR((endyr-selpar_n_yrs_wgted+1),endyr))/selpar_n_yrs_wgted;  
  log_F_dev_end_cH_D=sum(log_F_dev_cH_D((endyr-selpar_n_yrs_wgted+1),endyr))/selpar_n_yrs_wgted;
  log_F_dev_end_HB_D=sum(log_F_dev_HB_D((endyr-selpar_n_yrs_wgted+1),endyr))/selpar_n_yrs_wgted;
  log_F_dev_end_GR_D=sum(log_F_dev_GR_D((endyr-selpar_n_yrs_wgted+1),endyr))/selpar_n_yrs_wgted;
  F_end_L=sel_cH(endyr)*mfexp(log_avg_F_cH+log_F_dev_end_cH)+
          sel_cO(endyr)*mfexp(log_avg_F_cO+log_F_dev_end_cO)+
          sel_HB(endyr)*mfexp(log_avg_F_HB+log_F_dev_end_HB)+
          sel_GR(endyr)*mfexp(log_avg_F_GR+log_F_dev_end_GR);   
  F_end_D=sel_D(endyr)*mfexp(log_avg_F_cH_D+log_F_dev_end_cH_D)+
          sel_D(endyr)*mfexp(log_avg_F_HB_D+log_F_dev_end_HB_D)+
          sel_D(endyr)*mfexp(log_avg_F_GR_D+log_F_dev_end_GR_D);   
  F_end=F_end_L+F_end_D;
  F_end_apex=max(F_end);
  sel_wgted_tot=F_end/F_end_apex;
  sel_wgted_L=elem_prod(sel_wgted_tot, elem_div(F_end_L,F_end));
  sel_wgted_D=elem_prod(sel_wgted_tot, elem_div(F_end_D,F_end));
  wgt_wgted_L_denom=F_cH_prop+F_cO_prop+F_HB_prop+F_GR_prop;  
  wgt_wgted_L_klb=F_cH_prop/wgt_wgted_L_denom*wholewgt_cH_klb(endyr)+ 
				  F_cO_prop/wgt_wgted_L_denom*wholewgt_cO_klb(endyr)+ 
                  F_HB_prop/wgt_wgted_L_denom*wholewgt_HB_klb(endyr)+
                  F_GR_prop/wgt_wgted_L_denom*wholewgt_GR_klb(endyr);                          
  wgt_wgted_D_denom=F_cH_D_prop+F_HB_D_prop+F_GR_D_prop;  
  wgt_wgted_D_klb=F_cH_D_prop/wgt_wgted_D_denom*wholewgt_cH_D_klb(endyr)+ 
                  F_HB_D_prop/wgt_wgted_D_denom*wholewgt_HB_D_klb(endyr)+
                  F_GR_D_prop/wgt_wgted_D_denom*wholewgt_GR_D_klb(endyr);                
}

void model_parameters::get_msy(void)
{
  //compute values as functions of F
  for(ff=1; ff<=n_iter_msy; ff++)
  {
    //uses fishery-weighted F's
    Z_age_msy=0.0;
    F_L_age_msy=0.0;
    F_D_age_msy=0.0;
    F_L_age_msy=F_msy(ff)*sel_wgted_L;
    F_D_age_msy=F_msy(ff)*sel_wgted_D;    
    Z_age_msy=M+F_L_age_msy+F_D_age_msy;         
    N_age_msy(1)=1.0;
    for (iage=2; iage<=nages; iage++)
      {N_age_msy(iage)=N_age_msy(iage-1)*mfexp(-1.*Z_age_msy(iage-1));}
    N_age_msy(nages)=N_age_msy(nages)/(1.0-mfexp(-1.*Z_age_msy(nages)));
    N_age_msy_spawn(1,(nages-1))=elem_prod(N_age_msy(1,(nages-1)),
                                   mfexp((-1.*Z_age_msy(1,(nages-1)))*spawn_time_frac));                 
    N_age_msy_spawn(nages)=(N_age_msy_spawn(nages-1)*(mfexp(-1.*(Z_age_msy(nages-1)*(1.0-spawn_time_frac) + 
                            Z_age_msy(nages)*spawn_time_frac) )))/(1.0-mfexp(-1.*Z_age_msy(nages)));
    spr_msy(ff)=sum(elem_prod(N_age_msy_spawn,reprod));
    R_eq(ff)=SR_eq_func(R0, steep, spr_msy(1), spr_msy(ff), BiasCor, SR_switch);
    if (R_eq(ff)<dzero) {R_eq(ff)=dzero;}    
    N_age_msy*=R_eq(ff);
    N_age_msy_spawn*=R_eq(ff);
    for (iage=1; iage<=nages; iage++)
    {
      L_age_msy(iage)=N_age_msy(iage)*(F_L_age_msy(iage)/Z_age_msy(iage))*
                      (1.-mfexp(-1.*Z_age_msy(iage)));
      D_age_msy(iage)=N_age_msy(iage)*(F_D_age_msy(iage)/Z_age_msy(iage))*
                      (1.-mfexp(-1.0*Z_age_msy(iage)));                      
    }
    SSB_eq(ff)=sum(elem_prod(N_age_msy_spawn,reprod));
	B_eq(ff)=sum(elem_prod(N_age_msy,wgt_mt));
    L_eq_klb(ff)=sum(elem_prod(L_age_msy,wgt_wgted_L_klb)); //in whole weight
    L_eq_knum(ff)=sum(L_age_msy)/1000.0;  
    D_eq_klb(ff)=sum(elem_prod(D_age_msy,wgt_wgted_D_klb)); //in whole weight   
    D_eq_knum(ff)=sum(D_age_msy)/1000.0;    
  }  
  msy_klb_out=max(L_eq_klb); //msy in whole weight 
  for(ff=1; ff<=n_iter_msy; ff++)
  {
   if(L_eq_klb(ff) == msy_klb_out) 
      {    
        SSB_msy_out=SSB_eq(ff);
        B_msy_out=B_eq(ff);
        R_msy_out=R_eq(ff);
        msy_knum_out=L_eq_knum(ff);
        D_msy_knum_out=D_eq_knum(ff);
        D_msy_klb_out=D_eq_klb(ff);        
        F_msy_out=F_msy(ff);  
        spr_msy_out=spr_msy(ff);      
      }
  }
}

void model_parameters::get_per_recruit_stuff(void)
{
  //static per-recruit stuff
  for(iyear=styr; iyear<=endyr; iyear++)
  {
    N_age_spr(1)=1.0;
    for(iage=2; iage<=nages; iage++)
     {N_age_spr(iage)=N_age_spr(iage-1)*mfexp(-1.*Z(iyear,iage-1));}
    N_age_spr(nages)=N_age_spr(nages)/(1.0-mfexp(-1.*Z(iyear,nages)));    
    N_age_spr_spawn(1,(nages-1))=elem_prod(N_age_spr(1,(nages-1)),
                                mfexp(-1.*Z(iyear)(1,(nages-1))*spawn_time_frac));
    N_age_spr_spawn(nages)=(N_age_spr_spawn(nages-1)*
                          (mfexp(-1.*(Z(iyear)(nages-1)*(1.0-spawn_time_frac) + Z(iyear)(nages)*spawn_time_frac) )))
                          /(1.0-mfexp(-1.*Z(iyear)(nages)));           
    spr_static(iyear)=sum(elem_prod(N_age_spr_spawn,reprod))/spr_F0;
  }
  //compute SSB/R and YPR as functions of F
  for(ff=1; ff<=n_iter_spr; ff++)
  {
    //uses fishery-weighted F's, same as in MSY calculations
    Z_age_spr=0.0;
    F_L_age_spr=0.0;
    F_L_age_spr=F_spr(ff)*sel_wgted_L;
    F_D_age_spr=F_spr(ff)*sel_wgted_D;
    Z_age_spr=M+F_L_age_spr+F_D_age_spr;
    N_age_spr(1)=1.0;
    for (iage=2; iage<=nages; iage++)
     {N_age_spr(iage)=N_age_spr(iage-1)*mfexp(-1.*Z_age_spr(iage-1));}
    N_age_spr(nages)=N_age_spr(nages)/(1-mfexp(-1.*Z_age_spr(nages)));
    N_age_spr_spawn(1,(nages-1))=elem_prod(N_age_spr(1,(nages-1)),
                                   mfexp((-1.*Z_age_spr(1,(nages-1)))*spawn_time_frac));                 
    N_age_spr_spawn(nages)=(N_age_spr_spawn(nages-1)*
                          (mfexp(-1.*(Z_age_spr(nages-1)*(1.0-spawn_time_frac) + Z_age_spr(nages)*spawn_time_frac) )))
                          /(1.0-mfexp(-1.*Z_age_spr(nages)));
    spr_spr(ff)=sum(elem_prod(N_age_spr_spawn,reprod));
    L_spr(ff)=0.0;
    for (iage=1; iage<=nages; iage++)
    {
      L_age_spr(iage)=N_age_spr(iage)*(F_L_age_spr(iage)/Z_age_spr(iage))*
                      (1.-mfexp(-1.*Z_age_spr(iage)));
      L_spr(ff)+=L_age_spr(iage)*wgt_wgted_L_klb(iage)*1000.0; //in lb whole wgt
    }   
  }
  spr_ratio=spr_spr/spr_F0;
  F20_dum=min(fabs(spr_ratio-0.2));
  F30_dum=min(fabs(spr_ratio-0.3));
  F40_dum=min(fabs(spr_ratio-0.4));
  for(ff=1; ff<=n_iter_spr; ff++)
  {   
      if (fabs(spr_ratio(ff)-0.2)==F20_dum) {F20_out=F_spr(ff);}	  
	  if (fabs(spr_ratio(ff)-0.3)==F30_dum) {F30_out=F_spr(ff);}	  
	  if (fabs(spr_ratio(ff)-0.4)==F40_dum) {F40_out=F_spr(ff);}
  }
  rec=column(N,1);
  rec_mean=sum(rec(styr_rec_spr, endyr_rec_spr))/nyrs_rec_spr;
  R_F30_out=rec_mean;
  F_L_age_spr=F30_out*sel_wgted_L;
  F_D_age_spr=F30_out*sel_wgted_D;
  Z_age_spr=M+F_L_age_spr+F_D_age_spr;
  N_age_spr(1)=R_F30_out;
  for (iage=2; iage<=nages; iage++)
     {N_age_spr(iage)=N_age_spr(iage-1)*mfexp(-1.*Z_age_spr(iage-1));}
  N_age_spr(nages)=N_age_spr(nages)/(1-mfexp(-1.*Z_age_spr(nages)));
  N_age_spr_spawn(1,(nages-1))=elem_prod(N_age_spr(1,(nages-1)),
                                   mfexp((-1.*Z_age_spr(1,(nages-1)))*spawn_time_frac));                 
  N_age_spr_spawn(nages)=(N_age_spr_spawn(nages-1)*
                          (mfexp(-1.*(Z_age_spr(nages-1)*(1.0-spawn_time_frac) + Z_age_spr(nages)*spawn_time_frac) )))
                          /(1.0-mfexp(-1.*Z_age_spr(nages)));
  for (iage=1; iage<=nages; iage++)
    {
      L_age_F30(iage)=N_age_spr(iage)*(F_L_age_spr(iage)/Z_age_spr(iage))*
                      (1.-mfexp(-1.*Z_age_spr(iage)));
      D_age_F30(iage)=N_age_spr(iage)*(F_D_age_spr(iage)/Z_age_spr(iage))*
                      (1.-mfexp(-1.0*Z_age_spr(iage)));                      
    }
  SSB_F30_out=sum(elem_prod(N_age_spr_spawn,reprod));
  B_F30_out=sum(elem_prod(N_age_spr,wgt_mt));
  L_F30_klb_out=sum(elem_prod(L_age_F30,wgt_wgted_L_klb)); //in whole weight
  L_F30_knum_out=sum(L_age_F30)/1000.0;  
  D_F30_klb_out=sum(elem_prod(D_age_F30,wgt_wgted_D_klb)); //in whole weight   
  D_F30_knum_out=sum(D_age_F30)/1000.0;    
}

void model_parameters::get_miscellaneous_stuff(void)
{
  if(var_rec_dev>0.0)
   {sigma_rec_dev=sqrt(var_rec_dev);} //sample SD of predicted residuals (may not equal rec_sigma)  
   else{sigma_rec_dev=0.0;}
  len_cv=elem_div(len_sd,meanlen_TL);
  //compute total landings- and discards-at-age in 1000 fish and klb whole weight
  L_total_num.initialize();
  L_total_klb.initialize();
  L_total_knum_yr.initialize();
  L_total_klb_yr.initialize();  
  D_total_num.initialize();
  D_total_klb.initialize();
  D_total_knum_yr.initialize();
  D_total_klb_yr.initialize();
  D_cH_klb.initialize();
  D_HB_klb.initialize();
  D_GR_klb.initialize();
  for(iyear=styr; iyear<=endyr; iyear++)
  {
        L_total_klb_yr(iyear)=pred_cH_L_klb(iyear)+pred_cO_L_klb(iyear)+pred_HB_L_klb(iyear)+pred_GR_L_klb(iyear);
        L_total_knum_yr(iyear)=pred_cH_L_knum(iyear)+pred_cO_L_knum(iyear)+pred_HB_L_knum(iyear)+pred_GR_L_knum(iyear);
        B(iyear)=elem_prod(N(iyear),wgt_mt);
        totN(iyear)=sum(N(iyear));
        totB(iyear)=sum(B(iyear));   
        //if (iyear>=styr_cH_D && iyear<=endyr_cH_D)
	    if (iyear>endyr_selex_phase1 && iyear<=endyr)			
        {
         D_total_knum_yr(iyear)+=pred_cH_D_knum(iyear);
         D_total_klb_yr(iyear)+=pred_cH_D_klb(iyear);
         D_cH_klb(iyear)=elem_prod(D_cH_num(iyear),wholewgt_cH_D_klb(iyear));     //in 1000 lb 
        }
        //if (iyear>=styr_HB_D && iyear<=endyr_HB_D)
		if (iyear>endyr_selex_phase1 && iyear<=endyr)
        {
         D_total_knum_yr(iyear)+=pred_HB_D_knum(iyear);
         D_total_klb_yr(iyear)+=pred_HB_D_klb(iyear);
         D_HB_klb(iyear)=elem_prod(D_HB_num(iyear),wholewgt_HB_D_klb(iyear));     //in 1000 lb 
        }    
        //if (iyear>=styr_GR_D && iyear<=endyr_GR_D)
		if (iyear>=styr_GR_D && iyear<=endyr)	
        {
         D_total_knum_yr(iyear)+=pred_GR_D_knum(iyear);
         D_total_klb_yr(iyear)+=pred_GR_D_klb(iyear);
         D_GR_klb(iyear)=elem_prod(D_GR_num(iyear),wholewgt_GR_D_klb(iyear));     //in 1000 lb 
        }                          
  }
  L_total_num=L_cH_num+L_cO_num+L_HB_num+L_GR_num;   //landings at age in number fish
  L_total_klb=L_cH_klb+L_cO_klb+L_HB_klb+L_GR_klb;   //landings at age in klb whole weight
  D_total_num=(D_cH_num+D_HB_num+D_GR_num);          //discards at age in number fish
  D_total_klb=D_cH_klb+D_HB_klb+D_GR_klb;            //discards at age in klb whole weight
  //Time series of interest
  B(endyr+1)=elem_prod(N(endyr+1),wgt_mt);
  totN(endyr+1)=sum(N(endyr+1));
  totB(endyr+1)=sum(B(endyr+1));  
  SdS0=SSB/S0;
  Fend_mean_temp=1.0;
  for (iyear=1; iyear<=selpar_n_yrs_wgted; iyear++) {Fend_mean_temp*=Fapex(endyr-iyear+1);}
  Fend_mean=pow(Fend_mean_temp,(1.0/selpar_n_yrs_wgted));	  
  if(F_msy_out>0)
    {
      FdF_msy=Fapex/F_msy_out;
      FdF_msy_end=FdF_msy(endyr);
      FdF_msy_end_mean=Fend_mean/F_msy_out;
    }
  if(SSB_msy_out>0)
    {
      SdSSB_msy=SSB/SSB_msy_out;
      SdSSB_msy_end=SdSSB_msy(endyr);
    }  
	if(F30_out>0)
    {
	  FdF30=Fapex/F30_out;
	  FdF30_end_mean=Fend_mean/F30_out;
	}
  if(SSB_F30_out>0)
    {
      SdSSB_F30=SSB/SSB_F30_out;
	  Sdmsst_F30=SSB/(smsy2msst75*SSB_F30_out);
      SdSSB_F30_end=SdSSB_F30(endyr);
	  Sdmsst_F30_end=Sdmsst_F30(endyr);
    }  	
   //fill in log recruitment deviations for yrs they are nonzero
   for(iyear=styr_rec_dev; iyear<=endyr_rec_dev; iyear++)
     {log_rec_dev_output(iyear)=log_rec_dev(iyear);}
   //fill in log Nage deviations for ages they are nonzero (ages2+)
   for(iage=2; iage<=nages; iage++)
     {log_Nage_dev_output(iage)=log_Nage_dev(iage);}
}

void model_parameters::get_projection(void)
{
    switch(Fproj_switch){
       case 1: //F=Fcurrent
          F_reg_proj=Fend_mean;
          break;
       case 2: //F=Fmsy
          F_reg_proj=F_msy_out;
          break;
       case 3: //F=F30
          F_reg_proj=F30_out;
          break;     
       case 4: //F=F40
          F_reg_proj=F40_out;
          break;          		  
       default: // no such switch available
          cout << "Error in input: Projection switch Fproj_switch must be set to 1, 2, 3, or 4." << endl;
          cout << "Presently it is set to " << Fproj_switch <<"."<< endl;
          exit(0);          
   }
  N_proj(styr_proj)=N(endyr+1); //initial conditions computed previously
  for (iyear=styr_proj; iyear<=endyr_proj; iyear++) //recruitment follows S-R curve (with bias correction) exactly
  {     
        if (iyear<styr_regs) {F_proj(iyear)=Fend_mean;}
		else {F_proj(iyear)=Fproj_mult*F_reg_proj;}
		FL_age_proj=sel_wgted_L*F_proj(iyear);
		FD_age_proj=sel_wgted_D*F_proj(iyear);
        Z_proj(iyear)=M+FL_age_proj+FD_age_proj;
        N_spawn_proj(iyear)(1,nages)=elem_prod(N_proj(iyear)(1,nages),(mfexp(-1.*(Z_proj(iyear)(1,nages))*spawn_time_frac))); //peak spawning time
		SSB_proj(iyear)= sum(elem_prod(N_spawn_proj(iyear),reprod));
        B_proj(iyear)=sum(elem_prod(N_proj(iyear),wgt_mt)); //uses spawning weight
		for (iage=1; iage<=nages; iage++)
			{L_age_proj(iyear,iage)=N_proj(iyear,iage)*FL_age_proj(iage)*(1.-mfexp(-1.*Z_proj(iyear,iage)))/Z_proj(iyear,iage);
		     D_age_proj(iyear,iage)=N_proj(iyear,iage)*FD_age_proj(iage)*(1.-mfexp(-1.*Z_proj(iyear,iage)))/Z_proj(iyear,iage);
			}          
        L_knum_proj(iyear)=sum(L_age_proj(iyear))/1000.0;
	    D_knum_proj(iyear)=sum(D_age_proj(iyear))/1000.0;
	    L_klb_proj(iyear)=sum(elem_prod(L_age_proj(iyear),wgt_wgted_L_klb));     //in 1000 lb
        D_klb_proj(iyear)=sum(elem_prod(D_age_proj(iyear),wgt_wgted_D_klb));     //in 1000 lb
		if (iyear<endyr_proj) {
			N_proj(iyear+1,1)=BiasCor*SR_func(R0, steep, spr_F0, SSB_proj(iyear),SR_switch);
			N_proj(iyear+1)(2,nages)=++elem_prod(N_proj(iyear)(1,nages-1),(mfexp(-1.*Z_proj(iyear)(1,nages-1))));
			N_proj(iyear+1,nages)+=N_proj(iyear,nages)*mfexp(-1.*Z_proj(iyear,nages)); //plus group		
		}
  }
   R_proj=column(N_proj,1);                          
}

void model_parameters::evaluate_objective_function(void)
{
  //fval=square(xdum-9.0);
  fval=0.0;
  fval_data=0.0;  
  f_cH_cpue=0.0;
  f_cH_cpue=lk_lognormal(pred_cH_cpue, obs_cH_cpue, cH_cpue_cv, w_I_cH);
  fval+=f_cH_cpue;
  fval_data+=f_cH_cpue;  
  f_HB_cpue=0.0;
  f_HB_cpue=lk_lognormal(pred_HB_cpue, obs_HB_cpue, HB_cpue_cv, w_I_HB);
  fval+=f_HB_cpue;
  fval_data+=f_HB_cpue;  
  // f_GR_cpue=0.0;
  // f_GR_cpue=lk_lognormal(pred_GR_cpue, obs_GR_cpue, GR_cpue_cv, w_I_GR);
  // fval+=f_GR_cpue;
  // fval_data+=f_GR_cpue;  
  f_CVT_cpue=0.0;
  f_CVT_cpue=lk_lognormal(pred_CVT_cpue, obs_CVT_cpue, CVT_cpue_cv, w_I_CVT);
  fval+=f_CVT_cpue;
  fval_data+=f_CVT_cpue;  
  //f_cH_L in 1000 lb whole wgt  
  f_cH_L=lk_lognormal(pred_cH_L_klb(styr_cH_L,endyr_cH_L), obs_cH_L(styr_cH_L,endyr_cH_L),
                      cH_L_cv(styr_cH_L,endyr_cH_L), w_L);
  fval+=f_cH_L;
  fval_data+=f_cH_L;
  //f_cO_L in 1000 lb whole wgt  
  f_cO_L=lk_lognormal(pred_cO_L_klb(styr_cO_L,endyr_cO_L), obs_cO_L(styr_cO_L,endyr_cO_L),
                      cO_L_cv(styr_cO_L,endyr_cO_L), w_L);
  fval+=f_cO_L;
  fval_data+=f_cO_L;
  //f_HB_L in 1000 fish
  f_HB_L=lk_lognormal(pred_HB_L_knum(styr_HB_L,endyr_HB_L), obs_HB_L(styr_HB_L,endyr_HB_L), 
                      HB_L_cv(styr_HB_L,endyr_HB_L), w_L);
  fval+=f_HB_L;
  fval_data+=f_HB_L;  
  //f_GR_L in 1000 fish
  f_GR_L=lk_lognormal(pred_GR_L_knum(styr_GR_L,endyr_GR_L), obs_GR_L(styr_GR_L,endyr_GR_L), 
                      GR_L_cv(styr_GR_L,endyr_GR_L), w_L);
  fval+=f_GR_L;
  fval_data+=f_GR_L;  
  //f_cH_D in 1000 fish
  f_cH_D=lk_lognormal(pred_cH_D_knum(styr_cH_D,endyr_cH_D), obs_cH_D(styr_cH_D,endyr_cH_D), 
                      cH_D_cv(styr_cH_D,endyr_cH_D), w_D);
  fval+=f_cH_D;
  fval_data+=f_cH_D;  
  //f_HB_D in 1000 fish
  f_HB_D=lk_lognormal(pred_HB_D_knum(styr_HB_D,endyr_HB_D), obs_HB_D(styr_HB_D,endyr_HB_D), 
                      HB_D_cv(styr_HB_D,endyr_HB_D), w_D);
  fval+=f_HB_D;
  fval_data+=f_HB_D;  
  //f_GR_D in 1000 fish
  f_GR_D=lk_lognormal(pred_GR_D_knum(styr_GR_D,endyr_GR_D), obs_GR_D(styr_GR_D,endyr_GR_D), 
                      GR_D_cv(styr_GR_D,endyr_GR_D), w_D);
  fval+=f_GR_D;
  fval_data+=f_GR_D;  
  //f_cH_lenc
  //f_cH_lenc=lk_robust_multinomial(nsamp_cH_lenc, pred_cH_lenc, obs_cH_lenc, nyr_cH_lenc, double(nlenbins), minSS_cH_lenc, w_lc_cH);
  //f_cH_lenc=lk_logistic_normal(nsamp_cH_lenc, pred_cH_lenc, obs_cH_lenc, nyr_cH_lenc, double(nlenbins), minSS_cH_lenc);
  f_cH_lenc=lk_dirichlet_multinomial(nsamp_cH_lenc, pred_cH_lenc, obs_cH_lenc, nyr_cH_lenc, double(nlenbins), minSS_cH_lenc, log_dm_cH_lc);
  fval+=f_cH_lenc;
  fval_data+=f_cH_lenc;
  //f_cO_lenc
  //f_cO_lenc=lk_robust_multinomial(nsamp_cO_lenc, pred_cO_lenc, obs_cO_lenc, nyr_cO_lenc, double(nlenbins), minSS_cO_lenc, w_lc_cO);
  //f_cO_lenc=lk_logistic_normal(nsamp_cO_lenc, pred_cO_lenc, obs_cO_lenc, nyr_cO_lenc, double(nlenbins), minSS_cO_lenc);
  f_cO_lenc=lk_dirichlet_multinomial(nsamp_cO_lenc, pred_cO_lenc, obs_cO_lenc, nyr_cO_lenc, double(nlenbins), minSS_cO_lenc, log_dm_cO_lc);
  fval+=f_cO_lenc;
  fval_data+=f_cO_lenc;
  //f_HB_lenc
  //f_HB_lenc=lk_robust_multinomial(nsamp_HB_lenc, pred_HB_lenc, obs_HB_lenc, nyr_HB_lenc, double(nlenbins), minSS_HB_lenc, w_lc_HB);
  //f_HB_lenc=lk_logistic_normal(nsamp_HB_lenc, pred_HB_lenc, obs_HB_lenc, nyr_HB_lenc, double(nlenbins), minSS_HB_lenc);
  f_HB_lenc=lk_dirichlet_multinomial(nsamp_HB_lenc, pred_HB_lenc, obs_HB_lenc, nyr_HB_lenc, double(nlenbins), minSS_HB_lenc, log_dm_HB_lc);
  fval+=f_HB_lenc;
  fval_data+=f_HB_lenc;
  //f_GR_lenc
  //f_GR_lenc=lk_robust_multinomial(nsamp_GR_lenc, pred_GR_lenc, obs_GR_lenc, nyr_GR_lenc, double(nlenbins), minSS_GR_lenc, w_lc_GR);
  //f_GR_lenc=lk_logistic_normal(nsamp_GR_lenc, pred_GR_lenc, obs_GR_lenc, nyr_GR_lenc, double(nlenbins), minSS_GR_lenc);
  f_GR_lenc=lk_dirichlet_multinomial(nsamp_GR_lenc, pred_GR_lenc, obs_GR_lenc, nyr_GR_lenc, double(nlenbins), minSS_GR_lenc, log_dm_GR_lc);
  fval+=f_GR_lenc;
  fval_data+=f_GR_lenc;
  //f_HB_D_lenc
  //f_HB_D_lenc=lk_robust_multinomial(nsamp_HB_D_lenc, pred_HB_D_lenc, obs_HB_D_lenc, nyr_HB_D_lenc, double(nlenbins), minSS_HB_D_lenc, w_lc_HB_D);
  //f_HB_D_lenc=lk_logistic_normal(nsamp_HB_D_lenc, pred_HB_D_lenc, obs_HB_D_lenc, nyr_HB_D_lenc, double(nlenbins), minSS_HB_D_lenc);
  f_HB_D_lenc=lk_dirichlet_multinomial(nsamp_HB_D_lenc, pred_HB_D_lenc, obs_HB_D_lenc, nyr_HB_D_lenc, double(nlenbins), minSS_HB_D_lenc, log_dm_HB_D_lc);
  fval+=f_HB_D_lenc;
  fval_data+=f_HB_D_lenc;
  //f_CVT_lenc
  //f_CVT_lenc=lk_robust_multinomial(nsamp_CVT_lenc, pred_CVT_lenc, obs_CVT_lenc, nyr_CVT_lenc, double(nlenbins), minSS_CVT_lenc, w_lc_CVT);
  //f_CVT_lenc=lk_logistic_normal(nsamp_CVT_lenc, pred_CVT_lenc, obs_CVT_lenc, nyr_CVT_lenc, double(nlenbins), minSS_CVT_lenc);
  f_CVT_lenc=lk_dirichlet_multinomial(nsamp_CVT_lenc, pred_CVT_lenc, obs_CVT_lenc, nyr_CVT_lenc, double(nlenbins), minSS_CVT_lenc, log_dm_CVT_lc);
  fval+=f_CVT_lenc;
  fval_data+=f_CVT_lenc;
  //f_cH_agec
  //f_cH_agec=lk_robust_multinomial(nsamp_cH_agec, pred_cH_agec, obs_cH_agec, nyr_cH_agec, double(nages_agec), minSS_cH_agec, w_ac_cH);
  //f_cH_agec=lk_logistic_normal(nsamp_cH_agec, pred_cH_agec, obs_cH_agec, nyr_cH_agec, double(nages_agec), minSS_cH_agec);
  f_cH_agec=lk_dirichlet_multinomial(nsamp_cH_agec, pred_cH_agec, obs_cH_agec, nyr_cH_agec, double(nages_agec), minSS_cH_agec, log_dm_cH_ac);
  fval+=f_cH_agec;
  fval_data+=f_cH_agec;
  //f_HB_agec
  //f_HB_agec=lk_robust_multinomial(nsamp_HB_agec, pred_HB_agec, obs_HB_agec, nyr_HB_agec, double(nages_agec), minSS_HB_agec, w_ac_HB);
  //f_HB_agec=lk_logistic_normal(nsamp_HB_agec, pred_HB_agec, obs_HB_agec, nyr_HB_agec, double(nages_agec), minSS_HB_agec);
  f_HB_agec=lk_dirichlet_multinomial(nsamp_HB_agec, pred_HB_agec, obs_HB_agec, nyr_HB_agec, double(nages_agec), minSS_HB_agec, log_dm_HB_ac);
  fval+=f_HB_agec;
  fval_data+=f_HB_agec;
  //f_GR_agec
  //f_GR_agec=lk_robust_multinomial(nsamp_GR_agec, pred_GR_agec, obs_GR_agec, nyr_GR_agec, double(nages_agec), minSS_GR_agec, w_ac_GR);
  //f_GR_agec=lk_logistic_normal(nsamp_GR_agec, pred_GR_agec, obs_GR_agec, nyr_GR_agec, double(nages_agec), minSS_GR_agec);
  f_GR_agec=lk_dirichlet_multinomial(nsamp_GR_agec, pred_GR_agec, obs_GR_agec, nyr_GR_agec, double(nages_agec), minSS_GR_agec, log_dm_GR_ac);
  fval+=f_GR_agec;
  fval_data+=f_GR_agec;
  //f_CVT_agec
  //f_CVT_agec=lk_robust_multinomial(nsamp_CVT_agec, pred_CVT_agec, obs_CVT_agec, nyr_CVT_agec, double(nages_agec), minSS_CVT_agec, w_ac_CVT);
  //f_CVT_agec=lk_logistic_normal(nsamp_CVT_agec, pred_CVT_agec, obs_CVT_agec, nyr_CVT_agec, double(nages_agec), minSS_CVT_agec);
  f_CVT_agec=lk_dirichlet_multinomial(nsamp_CVT_agec, pred_CVT_agec, obs_CVT_agec, nyr_CVT_agec, double(nages_agec), minSS_CVT_agec, log_dm_CVT_ac);
  fval+=f_CVT_agec;
  fval_data+=f_CVT_agec;
  //Light penalty applied to log_Nage_dev for deviation from zero. If not estimated, this penalty equals zero.
  f_Nage_init=norm2(log_Nage_dev);        
  fval+=w_Nage_init*f_Nage_init;
  f_rec_dev=0.0;
  //rec_sigma_sq=square(rec_sigma);
  rec_logL_add=nyrs_rec*log(rec_sigma);
  f_rec_dev=(square(log_rec_dev(styr_rec_dev) + rec_sigma_sq/2.0)/(2.0*rec_sigma_sq));
  for(iyear=(styr_rec_dev+1); iyear<=endyr_rec_dev; iyear++)
  {f_rec_dev+=(square(log_rec_dev(iyear)-R_autocorr*log_rec_dev(iyear-1) + rec_sigma_sq/2.0)/
               (2.0*rec_sigma_sq));}
  f_rec_dev+=rec_logL_add;            
  fval+=w_rec*f_rec_dev;
  f_rec_dev_early=0.0; //possible extra constraint on early rec deviations
  if (w_rec_early>0.0)
    { if (styr_rec_dev<endyr_rec_phase1)
        {  
          for(iyear=styr_rec_dev; iyear<=endyr_rec_phase1; iyear++)
          //{f_rec_dev_early+=(square(log_rec_dev(iyear)-R_autocorr*log_rec_dev(iyear-1) + rec_sigma_sq/2.0)/
          //                  (2.0*rec_sigma_sq)) + rec_logL_add;}
          {f_rec_dev_early+=square(log_rec_dev(iyear));}
        }
  fval+=w_rec_early*f_rec_dev_early;
  }
  f_rec_dev_end=0.0; //possible extra constraint on ending rec deviations
  if (w_rec_end>0.0)
  { if (endyr_rec_phase2<endyr_rec_dev)
        {  
          for(iyear=(endyr_rec_phase2+1); iyear<=endyr_rec_dev; iyear++)
          //{f_rec_dev_end+=(square(log_rec_dev(iyear)-R_autocorr*log_rec_dev(iyear-1) + rec_sigma_sq/2.0)/
          //                 (2.0*rec_sigma_sq)) + rec_logL_add;}
          {f_rec_dev_end+=square(log_rec_dev(iyear));}
        }
      fval+=w_rec_end*f_rec_dev_end;
   }  
  //Ftune penalty: does not apply in last phase
  f_Ftune=0.0; 
  if (w_Ftune>0.0)
  {if (set_Ftune>0.0 && !last_phase()) {f_Ftune=square(Fapex(set_Ftune_yr)-set_Ftune);}
   fval+=w_Ftune*f_Ftune;
  }
  //Penalty if apical F exceeds 3.0
  f_fullF_constraint=0.0;
  if (w_fullF>0.0)
  {for (iyear=styr; iyear<=endyr; iyear++)
        {if(Fapex(iyear)>3.0) {f_fullF_constraint+=(mfexp(Fapex(iyear)-3.0)-1.0);}}
   fval+=w_fullF*f_fullF_constraint;
  }
 //Random walk components of fishery dependent indices
 f_HB_RWq_cpue=0.0;
 for (iyear=styr_HB_cpue; iyear<endyr_HB_cpue; iyear++)
     {f_HB_RWq_cpue+=square(q_RW_log_dev_HB(iyear))/(2.0*set_RWq_var);}
 fval+=f_HB_RWq_cpue;   
 f_cH_RWq_cpue=0.0;
 for (iyear=styr_cH_cpue; iyear<endyr_cH_cpue; iyear++)
     {f_cH_RWq_cpue+=square(q_RW_log_dev_cH(iyear))/(2.0*set_RWq_var);}
 fval+=f_cH_RWq_cpue;   
  f_priors=0.0; 
  f_priors+=neg_log_prior(len_cv_val,set_len_cv(5),set_len_cv(6),set_len_cv(7));
  f_priors+=neg_log_prior(steep,set_steep(5),set_log_R0(6),set_log_R0(7)); 
  f_priors+=neg_log_prior(log_R0,set_log_R0(5),set_log_R0(6),set_log_R0(7)); 
  f_priors+=neg_log_prior(R_autocorr,set_R_autocorr(5),set_R_autocorr(6),set_R_autocorr(7));
  f_priors+=neg_log_prior(rec_sigma,set_rec_sigma(5),set_rec_sigma(6),set_rec_sigma(7));
  //f_priors+=neg_log_prior(selpar_A50_cH1,set_selpar_A50_cH1(5), set_selpar_A50_cH1(6), set_selpar_A50_cH1(7));
  //f_priors+=neg_log_prior(selpar_slope_cH1,set_selpar_slope_cH1(5), set_selpar_slope_cH1(6), set_selpar_slope_cH1(7));
  f_priors+=neg_log_prior(selpar_A50_cH2,set_selpar_A50_cH2(5), set_selpar_A50_cH2(6), set_selpar_A50_cH2(7));
  f_priors+=neg_log_prior(selpar_slope_cH2,set_selpar_slope_cH2(5), set_selpar_slope_cH2(6), set_selpar_slope_cH2(7));
  f_priors+=neg_log_prior(selpar_A50_cH3,set_selpar_A50_cH3(5), set_selpar_A50_cH3(6), set_selpar_A50_cH3(7));
  f_priors+=neg_log_prior(selpar_slope_cH3,set_selpar_slope_cH3(5), set_selpar_slope_cH3(6), set_selpar_slope_cH3(7));
  f_priors+=neg_log_prior(selpar_A50_cO2,set_selpar_A50_cO2(5), set_selpar_A50_cO2(6), set_selpar_A50_cO2(7));
  f_priors+=neg_log_prior(selpar_A50_cO3,set_selpar_A50_cO3(5), set_selpar_A50_cO3(6), set_selpar_A50_cO3(7));
  f_priors+=neg_log_prior(selpar_slope_cO2,set_selpar_slope_cO2(5), set_selpar_slope_cO2(6), set_selpar_slope_cO2(7));
  f_priors+=neg_log_prior(selpar_A502_cO2,set_selpar_A502_cO2(5), set_selpar_A502_cO2(6), set_selpar_A502_cO2(7));
  f_priors+=neg_log_prior(selpar_slope2_cO2,set_selpar_slope2_cO2(5), set_selpar_slope2_cO2(6), set_selpar_slope2_cO2(7));
  f_priors+=neg_log_prior(selpar_A50_HB1,set_selpar_A50_HB1(5), set_selpar_A50_HB1(6), set_selpar_A50_HB1(7));
  f_priors+=neg_log_prior(selpar_slope_HB1,set_selpar_slope_HB1(5), set_selpar_slope_HB1(6), set_selpar_slope_HB1(7));
  f_priors+=neg_log_prior(selpar_A50_HB2,set_selpar_A50_HB2(5), set_selpar_A50_HB2(6), set_selpar_A50_HB2(7));
  f_priors+=neg_log_prior(selpar_slope_HB2,set_selpar_slope_HB2(5), set_selpar_slope_HB2(6), set_selpar_slope_HB2(7));
  f_priors+=neg_log_prior(selpar_A50_HB3,set_selpar_A50_HB3(5), set_selpar_A50_HB3(6), set_selpar_A50_HB3(7));
  f_priors+=neg_log_prior(selpar_slope_HB3,set_selpar_slope_HB3(5), set_selpar_slope_HB3(6), set_selpar_slope_HB3(7));
  f_priors+=neg_log_prior(selpar_A50_GR3,set_selpar_A50_GR3(5), set_selpar_A50_GR3(6), set_selpar_A50_GR3(7));
  f_priors+=neg_log_prior(selpar_slope_GR3,set_selpar_slope_GR3(5), set_selpar_slope_GR3(6), set_selpar_slope_GR3(7));
  f_priors+=neg_log_prior(selpar_A50_CVT,set_selpar_A50_CVT(5), set_selpar_A50_CVT(6), set_selpar_A50_CVT(7));
  f_priors+=neg_log_prior(selpar_slope_CVT,set_selpar_slope_CVT(5), set_selpar_slope_CVT(6), set_selpar_slope_CVT(7));
  f_priors+=neg_log_prior(selpar_A502_CVT,set_selpar_A502_CVT(5), set_selpar_A502_CVT(6), set_selpar_A502_CVT(7));
  f_priors+=neg_log_prior(selpar_slope2_CVT,set_selpar_slope2_CVT(5), set_selpar_slope2_CVT(6), set_selpar_slope2_CVT(7));
  f_priors+=neg_log_prior(selpar_age1logit_D,set_selpar_age1logit_D(5), set_selpar_age1logit_D(6), set_selpar_age1logit_D(7));
  f_priors+=neg_log_prior(log_q_cH,set_log_q_cH(5),set_log_q_cH(6),set_log_q_cH(7));
  f_priors+=neg_log_prior(log_q_HB,set_log_q_HB(5),set_log_q_HB(6),set_log_q_HB(7));
  //f_priors+=neg_log_prior(log_q_GR,set_log_q_GR(5),set_log_q_GR(6),set_log_q_GR(7));
  f_priors+=neg_log_prior(log_q_CVT,set_log_q_CVT(5),set_log_q_CVT(6),set_log_q_CVT(7));
  f_priors+=neg_log_prior(log_dm_cH_lc,set_log_dm_cH_lc(5),set_log_dm_cH_lc(6),set_log_dm_cH_lc(7));
  f_priors+=neg_log_prior(log_dm_cO_lc,set_log_dm_cO_lc(5),set_log_dm_cO_lc(6),set_log_dm_cO_lc(7));
  f_priors+=neg_log_prior(log_dm_HB_lc,set_log_dm_HB_lc(5),set_log_dm_HB_lc(6),set_log_dm_HB_lc(7));
  f_priors+=neg_log_prior(log_dm_GR_lc,set_log_dm_GR_lc(5),set_log_dm_GR_lc(6),set_log_dm_GR_lc(7));
  f_priors+=neg_log_prior(log_dm_HB_D_lc,set_log_dm_HB_D_lc(5),set_log_dm_HB_D_lc(6),set_log_dm_HB_D_lc(7));
  f_priors+=neg_log_prior(log_dm_CVT_lc,set_log_dm_CVT_lc(5),set_log_dm_CVT_lc(6),set_log_dm_CVT_lc(7));
  f_priors+=neg_log_prior(log_dm_cH_ac,set_log_dm_cH_ac(5),set_log_dm_cH_ac(6),set_log_dm_cH_ac(7));
  f_priors+=neg_log_prior(log_dm_HB_ac,set_log_dm_HB_ac(5),set_log_dm_HB_ac(6),set_log_dm_HB_ac(7));
  f_priors+=neg_log_prior(log_dm_GR_ac,set_log_dm_GR_ac(5),set_log_dm_GR_ac(6),set_log_dm_GR_ac(7));
  f_priors+=neg_log_prior(log_dm_CVT_ac,set_log_dm_CVT_ac(5),set_log_dm_CVT_ac(6),set_log_dm_CVT_ac(7));
  fval+=f_priors;
}

dvar_vector model_parameters::logistic(const dvar_vector& ages, const dvariable& A50, const dvariable& slope)
{
  //ages=vector of ages, A50=age at 50% selectivity, slope=rate of increase
  RETURN_ARRAYS_INCREMENT();
  dvar_vector Sel_Tmp(ages.indexmin(),ages.indexmax());
  Sel_Tmp=1./(1.+mfexp(-1.*slope*(ages-A50))); //logistic;  
  RETURN_ARRAYS_DECREMENT();
  return Sel_Tmp;
}

dvar_vector model_parameters::logistic_exponential(const dvar_vector& ages, const dvariable& A50, const dvariable& slope, const dvariable& sigma, const dvariable& joint)
{
  //ages=vector of ages, A50=age at 50% sel (ascending limb), slope=rate of increase, sigma=controls rate of descent (descending)                               
  //joint=age to join curves                                                                                                                                    
  RETURN_ARRAYS_INCREMENT();                                                                                                                                    
  dvar_vector Sel_Tmp(ages.indexmin(),ages.indexmax());                                                                                                         
  Sel_Tmp=1.0;                                                                                                                                                  
  for (iage=1; iage<=nages; iage++)                                                                                                                             
  {                                                                                                                                                             
   if (ages(iage)<joint) {Sel_Tmp(iage)=1./(1.+mfexp(-1.*slope*(ages(iage)-A50)));}                                                                             
   if (ages(iage)>joint){Sel_Tmp(iage)=mfexp(-1.*square((ages(iage)-joint)/sigma));}                                                                            
  }                                                                                                                                                             
  Sel_Tmp=Sel_Tmp/max(Sel_Tmp);                                                                                                                                 
  RETURN_ARRAYS_DECREMENT();                                                                                                                                    
  return Sel_Tmp;   
}

dvar_vector model_parameters::logistic_double(const dvar_vector& ages, const dvariable& A501, const dvariable& slope1, const dvariable& A502, const dvariable& slope2)
{
  //ages=vector of ages, A50=age at 50% selectivity, slope=rate of increase, A502=age at 50% decrease additive to A501, slope2=slope of decrease
  RETURN_ARRAYS_INCREMENT();
  dvar_vector Sel_Tmp(ages.indexmin(),ages.indexmax());
  Sel_Tmp=elem_prod( (1./(1.+mfexp(-1.*slope1*(ages-A501)))),(1.-(1./(1.+mfexp(-1.*slope2*(ages-(A501+A502)))))) );     
  Sel_Tmp=Sel_Tmp/max(Sel_Tmp);
  RETURN_ARRAYS_DECREMENT();
  return Sel_Tmp;
}

dvar_vector model_parameters::logistic_joint(const dvar_vector& ages, const dvariable& A501, const dvariable& slope1, const dvariable& A502, const dvariable& slope2, const dvariable& satval, const dvariable& joint)
{
  //ages=vector of ages, A501=age at 50% sel (ascending limb), slope1=rate of increase,A502=age at 50% sel (descending), slope1=rate of increase (ascending), 
  //satval=saturation value of descending limb, joint=location in age vector to join curves (may equal age or age + 1 if age-0 is included)
  RETURN_ARRAYS_INCREMENT();
  dvar_vector Sel_Tmp(ages.indexmin(),ages.indexmax());
  Sel_Tmp=1.0; 
  for (iage=1; iage<=nages; iage++)
  {
   if (double(iage)<joint) {Sel_Tmp(iage)=1./(1.+mfexp(-1.*slope1*(ages(iage)-A501)));}  
   if (double(iage)>joint){Sel_Tmp(iage)=1.0-(1.0-satval)/(1.+mfexp(-1.*slope2*(ages(iage)-A502)));}  
  }  
  Sel_Tmp=Sel_Tmp/max(Sel_Tmp);
  RETURN_ARRAYS_DECREMENT();
  return Sel_Tmp;
}

dvar_vector model_parameters::gaussian_double(const dvar_vector& ages, const dvariable& peak, const dvariable& top, const dvariable& ascwid, const dvariable& deswid, const dvariable& init, const dvariable& final)
{
  //ages=vector of ages, peak=ascending inflection location (as logistic), top=width of plateau, ascwid=ascent width (as log(width))
  //deswid=descent width (as log(width))
  RETURN_ARRAYS_INCREMENT();
  dvar_vector Sel_Tmp(ages.indexmin(),ages.indexmax());
  dvar_vector sel_step1(ages.indexmin(),ages.indexmax());
  dvar_vector sel_step2(ages.indexmin(),ages.indexmax());
  dvar_vector sel_step3(ages.indexmin(),ages.indexmax());
  dvar_vector sel_step4(ages.indexmin(),ages.indexmax());
  dvar_vector sel_step5(ages.indexmin(),ages.indexmax());
  dvar_vector sel_step6(ages.indexmin(),ages.indexmax());
  dvar_vector pars_tmp(1,6); dvar_vector sel_tmp_iq(1,2);
  pars_tmp(1)=peak;
  pars_tmp(2)=peak+1.0+(0.99*ages(nages)-peak-1.0)/(1.0+mfexp(-top));
  pars_tmp(3)=mfexp(ascwid);
  pars_tmp(4)=mfexp(deswid);
  pars_tmp(5)=1.0/(1.0+mfexp(-init));
  pars_tmp(6)=1.0/(1.0+mfexp(-final));
  sel_tmp_iq(1)=mfexp(-(square(ages(1)-pars_tmp(1))/pars_tmp(3)));
  sel_tmp_iq(2)=mfexp(-(square(ages(nages)-pars_tmp(2))/pars_tmp(4)));
  sel_step1=mfexp(-(square(ages-pars_tmp(1))/pars_tmp(3)));
  sel_step2=pars_tmp(5)+(1.0-pars_tmp(5))*(sel_step1-sel_tmp_iq(1))/(1.0-sel_tmp_iq(1));  
  sel_step3=mfexp(-(square(ages-pars_tmp(2))/pars_tmp(4)));
  sel_step4=1.0+(pars_tmp(6)-1.0)*(sel_step3-1.0)/(sel_tmp_iq(2)-1.0);
  sel_step5=1.0/ (1.0+mfexp(-(20.0* elem_div((ages-pars_tmp(1)), (1.0+sfabs(ages-pars_tmp(1)))) )));
  sel_step6=1.0/(1.0+mfexp(-(20.0*elem_div((ages-pars_tmp(2)),(1.0+sfabs(ages-pars_tmp(2)))) )));  
  Sel_Tmp=elem_prod(sel_step2,(1.0-sel_step5))+ 
          elem_prod(sel_step5,((1.0-sel_step6)+ elem_prod(sel_step4,sel_step6)) ); 
  Sel_Tmp=Sel_Tmp/max(Sel_Tmp);
  RETURN_ARRAYS_DECREMENT();
  return Sel_Tmp;
}

dvariable model_parameters::SR_func(const dvariable& R0, const dvariable& h, const dvariable& spr_F0, const dvariable& SSB, int func)
{
  //R0=virgin recruitment, h=steepness, spr_F0=spawners per recruit @ F=0, SSB=spawning biomass
  //func=1 for Beverton-Holt, 2 for Ricker
  RETURN_ARRAYS_INCREMENT();
  dvariable Recruits_Tmp;
  switch(func) {
    case 1: //Beverton-Holt
      Recruits_Tmp=((0.8*R0*h*SSB)/(0.2*R0*spr_F0*(1.0-h)+(h-0.2)*SSB));       
    break;
    case 2: //Ricker
      Recruits_Tmp=((SSB/spr_F0)*mfexp(h*(1-SSB/(R0*spr_F0))));       
    break;
  }
  RETURN_ARRAYS_DECREMENT();
  return Recruits_Tmp;
}

dvariable model_parameters::SR_eq_func(const dvariable& R0, const dvariable& h, const dvariable& spr_F0, const dvariable& spr_F, const dvariable& BC, int func)
{
  //R0=virgin recruitment, h=steepness, spr_F0=spawners per recruit @ F=0, spr_F=spawners per recruit @ F, BC=bias correction
  //func=1 for Beverton-Holt, 2 for Ricker
  RETURN_ARRAYS_INCREMENT();
  dvariable Recruits_Tmp;
  switch(func) {
    case 1: //Beverton-Holt
      Recruits_Tmp=(R0/((5.0*h-1.0)*spr_F))*(BC*4.0*h*spr_F-spr_F0*(1.0-h));    
    break;
    case 2: //Ricker
      Recruits_Tmp=R0/(spr_F/spr_F0)*(1.0+log(BC*spr_F/spr_F0)/h);      
    break;
  }
  RETURN_ARRAYS_DECREMENT();
  return Recruits_Tmp;
}

dvariable model_parameters::multinom_eff_N(const dvar_vector& pred_comp, const dvar_vector& obs_comp)
{
  //pred_comp=vector of predicted comps, obscomp=vector of observed comps
  dvariable EffN_Tmp; dvariable numer; dvariable denom;
  RETURN_ARRAYS_INCREMENT();
  numer=sum( elem_prod(pred_comp,(1.0-pred_comp)) );
  denom=sum( square(obs_comp-pred_comp) );
  if (denom>0.0) {EffN_Tmp=numer/denom;}
  else {EffN_Tmp=-missing;}                            
  RETURN_ARRAYS_DECREMENT();
  return EffN_Tmp;
}

dvariable model_parameters::lk_lognormal(const dvar_vector& pred, const dvar_vector& obs, const dvar_vector& cv, const dvariable& wgt_dat)
{
  //pred=vector of predicted vals, obs=vector of observed vals, cv=vector of CVs in arithmetic space, wgt_dat=constant scaling of CVs
  //small_number is small value to avoid log(0) during search
  RETURN_ARRAYS_INCREMENT();
  dvariable LkvalTmp;
  dvariable small_number=0.0001;
  dvar_vector var(cv.indexmin(),cv.indexmax()); //variance in log space
  var=log(1.0+square(cv/wgt_dat));   // convert cv in arithmetic space to variance in log space
  LkvalTmp=sum(0.5*elem_div(square(log(elem_div((pred+small_number),(obs+small_number)))),var) );
  RETURN_ARRAYS_DECREMENT();
  return LkvalTmp;
}

dvariable model_parameters::lk_multinomial(const dvar_vector& nsamp, const dvar_matrix& pred_comp, const dvar_matrix& obs_comp, const double& ncomp, const double& minSS, const dvariable& wgt_dat)
{
  //nsamp=vector of N's, pred_comp=matrix of predicted comps, obs_comp=matrix of observed comps, ncomp = number of yrs in matrix, minSS=min N threshold, wgt_dat=scaling of N's
  RETURN_ARRAYS_INCREMENT();
  dvariable LkvalTmp;
  dvariable small_number=0.0001;
  LkvalTmp=0.0;
  for (int ii=1; ii<=ncomp; ii++)
  {if (nsamp(ii)>=minSS)
    {LkvalTmp-=wgt_dat*nsamp(ii)*sum(elem_prod((obs_comp(ii)+small_number),
               log(elem_div((pred_comp(ii)+small_number), (obs_comp(ii)+small_number)))));
    }
  }  
  RETURN_ARRAYS_DECREMENT();
  return LkvalTmp;
}

dvariable model_parameters::lk_robust_multinomial(const dvar_vector& nsamp, const dvar_matrix& pred_comp, const dvar_matrix& obs_comp, const double& ncomp, const dvariable& mbin, const double& minSS, const dvariable& wgt_dat)
{
  //nsamp=vector of N's, pred_comp=matrix of predicted comps, obs_comp=matrix of observed comps, ncomp = number of yrs in matrix, mbin=number of bins, minSS=min N threshold, wgt_dat=scaling of N's
  RETURN_ARRAYS_INCREMENT();
  dvariable LkvalTmp;
  dvariable small_number=0.0001;
  LkvalTmp=0.0;
  dvar_matrix Eprime=elem_prod((1.0-obs_comp), obs_comp)+0.1/mbin; //E' of Francis 2011, p.1131  
  dvar_vector nsamp_wgt=nsamp*wgt_dat;
  //cout<<nsamp_wgt<<endl;
  for (int ii=1; ii<=ncomp; ii++)
  {if (nsamp(ii)>=minSS)
    {LkvalTmp+= sum(0.5*log(Eprime(ii))-log(small_number+mfexp(elem_div((-square(obs_comp(ii)-pred_comp(ii))) , (Eprime(ii)*2.0/nsamp_wgt(ii)) ))) );
    }
  }  
  RETURN_ARRAYS_DECREMENT();
  return LkvalTmp;
}

dvariable model_parameters::lk_dirichlet_multinomial(const dvar_vector& nsamp, const dvar_matrix& pred_comp, const dvar_matrix& obs_comp, const double& ncomp, const dvariable& mbin, const double& minSS, const dvariable& log_dir_par)
{
  //nsamp=vector of N's, pred_comp=matrix of predicted comps, obs_comp=matrix of observed comps, ncomp = number of yrs in matrix, mbin=number of bins, minSS=min N threshold, wgt_dat=scaling of N's
  RETURN_ARRAYS_INCREMENT();
  dvariable LkvalTmp;
  LkvalTmp=0.0; 
  dvar_vector nsamp_adjust=nsamp*mfexp(log_dir_par);
  //dvar_vector nsamp_adjust=mfexp(log_dir_par);
  for (int ii=1; ii<=ncomp; ii++)
  {
	if (nsamp(ii)>=minSS)
    {
		LkvalTmp-=gammln(nsamp_adjust(ii))-gammln(nsamp(ii)+nsamp_adjust(ii));
		LkvalTmp-=sum(gammln(nsamp(ii)*obs_comp(ii)+nsamp_adjust(ii)*pred_comp(ii)));
        LkvalTmp+=sum(gammln(nsamp_adjust(ii)*pred_comp(ii)));		
    }
  }  
  RETURN_ARRAYS_DECREMENT();
  return LkvalTmp;
}

dvariable model_parameters::lk_logistic_normal(const dvar_vector& nsamp, const dvar_matrix& pred_comp, const dvar_matrix& obs_comp, const double& ncomp, const dvariable& mbin, const double& minSS)
{
  //nsamp=vector of N's, pred_comp=matrix of predicted comps, obs_comp=matrix of observed comps, ncomp = number of yrs in matrix, mbin=number of bins, minSS=min N threshold
  RETURN_ARRAYS_INCREMENT();
  dvariable LkvalTmp;
  dvariable small_number=0.0001;
  LkvalTmp=0.0;
  dvar_matrix nu=pred_comp+0.0;
  dvar_matrix pred_plus=pred_comp+small_number;
  dvar_matrix obs_plus=obs_comp+small_number;
  dvariable nu_mean;
  dvariable nu_sum_sq;
  dvariable tau_hat_sq;
  dvariable year_count; //keeps track of years included in likelihood (i.e., that meet the sample size requirement)
  LkvalTmp=0.0;
  nu_sum_sq=0.0;
  year_count=0.0;
  for (int ii=1; ii<=ncomp; ii++)
  {if (nsamp(ii)>=minSS)
    {
		year_count+=1.0;
		nu_mean=sum( log(obs_plus(ii))-log(pred_plus(ii))  )/mbin;	//year-specific mean log residual
		for (int jj=1; jj<=mbin;jj++)
		{
			nu(ii,jj) = log(obs_plus(ii,jj)) - log(pred_plus(ii,jj)) - nu_mean;
			nu_sum_sq += square(nu(ii,jj));
		}
    }
  }  
  if (year_count>0.0)
  {
	  tau_hat_sq = nu_sum_sq/((mbin-1.0)*year_count);
	  LkvalTmp = (mbin-1.0)*year_count*log(tau_hat_sq);
  }
  RETURN_ARRAYS_DECREMENT();
  return LkvalTmp;
}

dvariable model_parameters::neg_log_prior(dvariable pred, const double& prior, dvariable var, int pdf)
{
  //prior=prior point estimate, var=variance (if negative, treated as CV in arithmetic space), pred=predicted value, pdf=prior type (1=none, 2=lognormal, 3=normal, 4=beta)
    dvariable LkvalTmp;
    dvariable alpha, beta, ab_iq;
    dvariable big_number=1e10;
    LkvalTmp=0.0;
    // compute generic pdf's
    switch(pdf) {
        case 1: //option to turn off prior
          LkvalTmp=0.0;
          break;
        case 2: // lognormal
          if(prior<=0.0) cout << "YIKES: Don't use a lognormal distn for a negative prior" << endl;
          else if(pred<=0) LkvalTmp=big_number=1e10;
          else {
            if(var<0.0) var=log(1.0+var*var) ;      // convert cv to variance on log scale
            LkvalTmp= 0.5*( square(log(pred/prior))/var + log(var) );
          }
        break;
        case 3: // normal
          if(var<0.0 && prior!=0.0) var=square(var*prior);       // convert cv to variance on observation scale
          else if(var<0.0 && prior==0.0) var=-var;               // cv not really appropriate if prior value equals zero
          LkvalTmp= 0.5*( square(pred-prior)/var + log(var) );
          break;
        case 4: // beta
          if(var<0.0) var=square(var*prior);          // convert cv to variance on observation scale
          if(prior<=0.0 || prior>=1.0) cout << "YIKES: Don't use a beta distn for a prior outside (0,1)" << endl;
          ab_iq=prior*(1.0-prior)/var - 1.0; alpha=prior*ab_iq; beta=(1.0-prior)*ab_iq;
          if(pred>=0 && pred<=1) LkvalTmp= (1.0-alpha)*log(pred)+(1.0-beta)*log(1.0-pred)-gammln(alpha+beta)+gammln(alpha)+gammln(beta);
          else LkvalTmp=big_number;
          break;
        default: // no such prior pdf currently available
          cout << "The prior must be either 1(lognormal), 2(normal), or 3(beta)." << endl;
          cout << "Presently it is " << pdf << endl;
          exit(0);
    }
    return LkvalTmp;
}

dvariable model_parameters::sdnr_multinomial(const double& ncomp, const dvar_vector& ages, const dvar_vector& nsamp, 
                                    const dvar_matrix& pred_comp, const dvar_matrix& obs_comp, const dvariable& wgt_dat)
{
  //ncomp=number of years of data, ages=vector of ages, nsamp=vector of N's, 
  //pred_comp=matrix of predicted comps, obs_comp=matrix of observed comps, wgt_dat=likelihood weight for data source
  RETURN_ARRAYS_INCREMENT();
  dvariable SdnrTmp;
  dvar_vector o(1,ncomp);  
  dvar_vector p(1,ncomp);  
  dvar_vector ose(1,ncomp);  
  dvar_vector res(1,ncomp);
  SdnrTmp=0.0;
  for (int ii=1; ii<=ncomp; ii++)
  {
    o(ii)=sum(elem_prod(ages,obs_comp(ii)));
    p(ii)=sum(elem_prod(ages,pred_comp(ii)));
    ose(ii)=sqrt((sum(elem_prod(square(ages),pred_comp(ii)))-square(p(ii)))/(nsamp(ii)*wgt_dat));
  }
  res=elem_div((o-p),ose); 
  SdnrTmp=sqrt(sum(square(res-(sum(res)/ncomp))/(ncomp-1.0))); 
  RETURN_ARRAYS_DECREMENT();
  return SdnrTmp;
}

dvariable model_parameters::sdnr_lognormal(const dvar_vector& pred, const dvar_vector& obs, const dvar_vector& cv, const dvariable& wgt_dat)
{
  //nyr=number of years of data, pred=vector of predicted data, obs=vector of observed data, cv=vector of cv's, wgt_dat=likelihood weight for data source
  RETURN_ARRAYS_INCREMENT();
  dvariable SdnrTmp;
  dvariable small_number=0.00001;
  dvariable n;
  dvar_vector res(cv.indexmin(),cv.indexmax());
  SdnrTmp=0.0;
  res=elem_div(log(elem_div(obs+small_number,pred+small_number)),sqrt(log(1+square(cv/wgt_dat))));
  n=cv.indexmax()-cv.indexmin()+1;
  SdnrTmp=sqrt(sum(square(res-(sum(res)/n))/(n-1.0))); 
  RETURN_ARRAYS_DECREMENT();
  return SdnrTmp; 
}

void model_parameters::report(const dvector& gradients)
{
 adstring ad_tmp=initial_params::get_reportfile_name();
  ofstream report((char*)(adprogram_name + ad_tmp));
  if (!report)
  {
    cerr << "error trying to open report file"  << adprogram_name << ".rep";
    return;
  }
  if (last_phase())  
  {
      cout<<"start report"<<endl;
      // cout<<"xdum = "<<xdum<<endl;
	  get_weighted_current();
      // cout<<"got weighted"<<endl;
      get_msy();
      // cout<<"got msy"<<endl;
      get_per_recruit_stuff();
      // cout<<"got per recruit"<<endl;  
      get_miscellaneous_stuff();
      // cout<<"got misc stuff"<<endl;
	  get_projection();
	  // cout<<"got projection"<<endl;
      grad_max=objective_function_value::pobjfun->gmax;
      time(&finish);
	  elapsed_time=difftime(finish,start);
	  hour=long(elapsed_time)/3600;
	  minute=long(elapsed_time)%3600/60;
	  second=(long(elapsed_time)%3600)%60;
	  cout<<endl<<endl<<"*******************************************"<<endl;
	  cout<<"--Start time: "<<ctime(&start)<<endl;
	  cout<<"--Finish time: "<<ctime(&finish)<<endl;
	  cout<<"--Runtime: ";
	  cout<<hour<<" hours, "<<minute<<" minutes, "<<second<<" seconds"<<endl;
	  cout << "--TotalLikelihood: " << fval << endl;
      cout<<"--Final gradient: "<<objective_function_value::pobjfun->gmax << endl;
	  cout<<"*******************************************"<<endl;
      cout <<endl;     
      cout << "><>--><>--><>--><>--><>--><>--><>--><>--><>--><>"  <<endl;
      //cout << "BC Fmsy=" << F_msy_out<< "   BC SSBmsy=" << SSB_msy_out <<endl;
      cout <<"F status="<<FdF_msy_end<<endl;
      cout <<"Pop status="<<SdSSB_msy_end<<endl;
      cout << "h="<<steep<<"   R0="<<R0<<endl;
      //cout << "xdum " << xdum << endl;
      cout << "><>--><>--><>--><>--><>--><>--><>--><>--><>--><>"  <<endl;  
      report << "TotalLikelihood " << fval << endl;
      report << "N" << endl;
      report << N<<endl;
      report << "F" << endl;
      report << F <<endl;
      report << "prob_belowsizelim_block3" << endl;
	  report<<prob_belowsizelim_block3<<endl;
      sdnr_lc_cH=sdnr_multinomial(nyr_cH_lenc, lenbins, nsamp_cH_lenc, pred_cH_lenc, obs_cH_lenc, w_lc_cH);  
      sdnr_lc_cO=sdnr_multinomial(nyr_cO_lenc, lenbins, nsamp_cO_lenc, pred_cO_lenc, obs_cO_lenc, w_lc_cO);  	  
      sdnr_lc_HB=sdnr_multinomial(nyr_HB_lenc, lenbins, nsamp_HB_lenc, pred_HB_lenc, obs_HB_lenc, w_lc_HB); 
	  sdnr_lc_GR=sdnr_multinomial(nyr_GR_lenc, lenbins, nsamp_GR_lenc, pred_GR_lenc, obs_GR_lenc, w_lc_GR); 
      sdnr_lc_HB_D=sdnr_multinomial(nyr_HB_D_lenc, lenbins, nsamp_HB_D_lenc, pred_HB_D_lenc, obs_HB_D_lenc, w_lc_HB_D); 
	  sdnr_lc_CVT=sdnr_multinomial(nyr_CVT_lenc, lenbins, nsamp_CVT_lenc, pred_CVT_lenc, obs_CVT_lenc, w_lc_CVT); 
      sdnr_ac_cH=sdnr_multinomial(nyr_cH_agec, agebins_agec, nsamp_cH_agec, pred_cH_agec, obs_cH_agec, w_ac_cH);  
      sdnr_ac_HB=sdnr_multinomial(nyr_HB_agec, agebins_agec, nsamp_HB_agec, pred_HB_agec, obs_HB_agec, w_ac_HB);  
	  sdnr_ac_GR=sdnr_multinomial(nyr_GR_agec, agebins_agec, nsamp_GR_agec, pred_GR_agec, obs_GR_agec, w_ac_GR);  
      sdnr_ac_CVT=sdnr_multinomial(nyr_CVT_agec, agebins_agec, nsamp_CVT_agec, pred_CVT_agec, obs_CVT_agec, w_ac_CVT);  
      sdnr_I_cH=sdnr_lognormal(pred_cH_cpue, obs_cH_cpue, cH_cpue_cv, w_I_cH);
      sdnr_I_HB=sdnr_lognormal(pred_HB_cpue, obs_HB_cpue, HB_cpue_cv, w_I_HB);
      //sdnr_I_GR=sdnr_lognormal(pred_GR_cpue, obs_GR_cpue, GR_cpue_cv, w_I_GR);
      sdnr_I_CVT=sdnr_lognormal(pred_CVT_cpue, obs_CVT_cpue, CVT_cpue_cv, w_I_CVT);  
      //#################################################################################################
      //##  Passing parameters to vector for bounds check plotting
      //################################################################################################# 
       Linf_out(8)=Linf; Linf_out(1,7)=set_Linf; 
       K_out(8)=K; K_out(1,7)=set_K;
       t0_out(8)=t0; t0_out(1,7)=set_t0;
       len_cv_val_out(8)=len_cv_val; len_cv_val_out(1,7)=set_len_cv;
       log_R0_out(8)=log_R0; log_R0_out(1,7)=set_log_R0;
       M_constant_out(8)=M_constant; M_constant_out(1,7)=set_M_constant;
       steep_out(8)=steep; steep_out(1,7)=set_steep;
       rec_sigma_out(8)=rec_sigma; rec_sigma_out(1,7)=set_rec_sigma;
       R_autocorr_out(8)=R_autocorr; R_autocorr_out(1,7)=set_R_autocorr;
	   log_dm_cH_lc_out(8)=log_dm_cH_lc; log_dm_cH_lc_out(1,7)=set_log_dm_cH_lc;
	   log_dm_cO_lc_out(8)=log_dm_cO_lc; log_dm_cO_lc_out(1,7)=set_log_dm_cO_lc;
	   log_dm_HB_lc_out(8)=log_dm_HB_lc; log_dm_HB_lc_out(1,7)=set_log_dm_HB_lc;
	   log_dm_GR_lc_out(8)=log_dm_GR_lc; log_dm_GR_lc_out(1,7)=set_log_dm_GR_lc;
	   log_dm_HB_D_lc_out(8)=log_dm_HB_D_lc; log_dm_HB_D_lc_out(1,7)=set_log_dm_HB_D_lc;
	   log_dm_CVT_lc_out(8)=log_dm_CVT_lc; log_dm_CVT_lc_out(1,7)=set_log_dm_CVT_lc;
	   log_dm_cH_ac_out(8)=log_dm_cH_ac; log_dm_cH_ac_out(1,7)=set_log_dm_cH_ac;
	   log_dm_HB_ac_out(8)=log_dm_HB_ac; log_dm_HB_ac_out(1,7)=set_log_dm_HB_ac;
	   log_dm_GR_ac_out(8)=log_dm_GR_ac; log_dm_GR_ac_out(1,7)=set_log_dm_GR_ac;
	   log_dm_CVT_ac_out(8)=log_dm_CVT_ac; log_dm_CVT_ac_out(1,7)=set_log_dm_CVT_ac;
       //selpar_A50_cH1_out(8)=selpar_A50_cH1; selpar_A50_cH1_out(1,7)=set_selpar_A50_cH1;
       //selpar_slope_cH1_out(8)=selpar_slope_cH1; selpar_slope_cH1_out(1,7)=set_selpar_slope_cH1;
       selpar_A50_cH2_out(8)=selpar_A50_cH2; selpar_A50_cH2_out(1,7)=set_selpar_A50_cH2;
       selpar_slope_cH2_out(8)=selpar_slope_cH2; selpar_slope_cH2_out(1,7)=set_selpar_slope_cH2;
       selpar_A50_cH3_out(8)=selpar_A50_cH3; selpar_A50_cH3_out(1,7)=set_selpar_A50_cH3;
       selpar_slope_cH3_out(8)=selpar_slope_cH3; selpar_slope_cH3_out(1,7)=set_selpar_slope_cH3;
	   selpar_A50_cO2_out(8)=selpar_A50_cO2; selpar_A50_cO2_out(1,7)=set_selpar_A50_cO2;
       selpar_A50_cO3_out(8)=selpar_A50_cO3; selpar_A50_cO3_out(1,7)=set_selpar_A50_cO3;
       selpar_slope_cO2_out(8)=selpar_slope_cO2; selpar_slope_cO2_out(1,7)=set_selpar_slope_cO2;
       selpar_A502_cO2_out(8)=selpar_A502_cO2; selpar_A502_cO2_out(1,7)=set_selpar_A502_cO2;
       selpar_slope2_cO2_out(8)=selpar_slope2_cO2; selpar_slope2_cO2_out(1,7)=set_selpar_slope2_cO2;	  
       selpar_A50_HB1_out(8)=selpar_A50_HB1; selpar_A50_HB1_out(1,7)=set_selpar_A50_HB1;
       selpar_slope_HB1_out(8)=selpar_slope_HB1; selpar_slope_HB1_out(1,7)=set_selpar_slope_HB1;
       selpar_A50_HB2_out(8)=selpar_A50_HB2; selpar_A50_HB2_out(1,7)=set_selpar_A50_HB2;
       selpar_slope_HB2_out(8)=selpar_slope_HB2; selpar_slope_HB2_out(1,7)=set_selpar_slope_HB2;
       selpar_A50_HB3_out(8)=selpar_A50_HB3; selpar_A50_HB3_out(1,7)=set_selpar_A50_HB3;
       selpar_slope_HB3_out(8)=selpar_slope_HB3; selpar_slope_HB3_out(1,7)=set_selpar_slope_HB3;
	   selpar_A50_GR3_out(8)=selpar_A50_GR3; selpar_A50_GR3_out(1,7)=set_selpar_A50_GR3;
       selpar_slope_GR3_out(8)=selpar_slope_GR3; selpar_slope_GR3_out(1,7)=set_selpar_slope_GR3;
       selpar_A50_CVT_out(8)=selpar_A50_CVT; selpar_A50_CVT_out(1,7)=set_selpar_A50_CVT;
       selpar_slope_CVT_out(8)=selpar_slope_CVT; selpar_slope_CVT_out(1,7)=set_selpar_slope_CVT;
       selpar_A502_CVT_out(8)=selpar_A502_CVT; selpar_A502_CVT_out(1,7)=set_selpar_A502_CVT;
       selpar_slope2_CVT_out(8)=selpar_slope2_CVT; selpar_slope2_CVT_out(1,7)=set_selpar_slope2_CVT;
	   selpar_age1logit_D_out(8)=selpar_age1logit_D; selpar_age1logit_D_out(1,7)=set_selpar_age1logit_D;
       log_q_cH_out(8)=log_q_cH; log_q_cH_out(1,7)=set_log_q_cH;
       log_q_HB_out(8)=log_q_HB; log_q_HB_out(1,7)=set_log_q_HB;
       //log_q_GR_out(8)=log_q_GR; log_q_GR_out(1,7)=set_log_q_GR;
       log_q_CVT_out(8)=log_q_CVT; log_q_CVT_out(1,7)=set_log_q_CVT;
       log_avg_F_cH_out(8)=log_avg_F_cH; log_avg_F_cH_out(1,7)=set_log_avg_F_cH;
	   log_avg_F_cO_out(8)=log_avg_F_cO; log_avg_F_cO_out(1,7)=set_log_avg_F_cO;
       log_avg_F_HB_out(8)=log_avg_F_HB; log_avg_F_HB_out(1,7)=set_log_avg_F_HB;
       log_avg_F_GR_out(8)=log_avg_F_GR; log_avg_F_GR_out(1,7)=set_log_avg_F_GR;       
       log_avg_F_cH_D_out(8)=log_avg_F_cH_D; log_avg_F_cH_D_out(1,7)=set_log_avg_F_cH_D;
       log_avg_F_HB_D_out(8)=log_avg_F_HB_D; log_avg_F_HB_D_out(1,7)=set_log_avg_F_HB_D;
       log_avg_F_GR_D_out(8)=log_avg_F_GR_D; log_avg_F_GR_D_out(1,7)=set_log_avg_F_GR_D;
       log_rec_dev_out(styr_rec_dev, endyr_rec_dev)=log_rec_dev;
       log_F_dev_cH_out(styr_cH_L,endyr_cH_L)=log_F_dev_cH;
	   log_F_dev_cO_out(styr_cO_L,endyr_cO_L)=log_F_dev_cO;
       log_F_dev_HB_out(styr_HB_L,endyr_HB_L)=log_F_dev_HB;
       log_F_dev_GR_out(styr_GR_L,endyr_GR_L)=log_F_dev_GR;
       log_F_dev_cH_D_out(styr_cH_D,endyr_cH_D)=log_F_dev_cH_D;
       log_F_dev_HB_D_out(styr_HB_D,endyr_HB_D)=log_F_dev_HB_D;
       log_F_dev_GR_D_out(styr_GR_D,endyr_GR_D)=log_F_dev_GR_D;
   #include "rg_make_Robject4.cxx"   // write the R-compatible report
  } //endl last phase loop     
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{}

void model_parameters::final_calcs(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;

int main(int argc,char * argv[])
{
    ad_set_new_handler();
  ad_exit=&ad_boundf;
  time(&start);
  arrmblsize=20000000;
  gradient_structure::set_MAX_NVAR_OFFSET(1600);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(2000000);
  gradient_structure::set_CMPDIF_BUFFER_SIZE(2000000);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(10000);
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
    if (!arrmblsize) arrmblsize=15000000;
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}
