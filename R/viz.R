get_glmnet_objs =
        function(glmnet_wflow) {
                
                # extract engine
                glmnet_engine =
                        glmnet_wflow %>%
                        extract_fit_engine()
                
                # extract parsnip
                glmnet_parsnip =
                        glmnet_wflow %>%
                        extract_fit_parsnip()
                
                
                # return both
                list("engine" = glmnet_engine,
                     "parsnip" = glmnet_parsnip)
                
        }

get_glmnet_coefs = function(glmnet_objs,
                            type = 'parsnip',
                            remove_intercept = T,
                            return_zeroes = T) {
        
        coefs = 
                glmnet_objs |>
                pluck(type) |>
                tidy(return_zeroes = return_zeroes)
        
        if (remove_intercept == T) {
                coefs =
                        coefs |>
                        filter(term != "(Intercept)")
        } 
        
        coefs
}

trace_plot = function(workflow,
                             upper_estimate = 0.05,
                             lower_estimate = -0.5,
                             minlength = 50,
                             max.overlaps = 25) {
        
        
        glmnet_objs = 
                workflow |>
                get_glmnet_objs()
        
        # get lambda
        lambda =
                glmnet_objs|> 
                pluck("parsnip") |> 
                tidy() |> 
                pull(penalty) |> 
                unique()
        
        coefs = 
                glmnet_objs |>
                get_glmnet_coefs(
                        type = 'engine'
                )
        
        plot_coefs = 
                coefs |>
                mutate(label_left =
                               case_when(
                                       lambda == min(lambda) & abs(estimate) > upper_estimate ~ term)) %>%
                group_by(term) %>%
                mutate(label_right =
                               case_when(
                                       lambda == max(lambda) & estimate > lower_estimate ~ term)) %>%
                ungroup()
        
        plot_coefs |>
                ggplot(aes(x=log(lambda),
                           y=estimate,
                           group = term))+
                geom_line(alpha = 0.5,
                          color = 'grey60')+
                geom_vline(xintercept = log(lambda),
                           linetype = 'dotted',
                           alpha = 0.5)+
                guides(color = 'none')+
                theme_minimal()+
                ggrepel::geom_text_repel(
                        aes(label = label_left),
                        max.overlaps = max.overlaps,
                        size = 2,
                        direction = "y",
                        hjust =2,
                        segment.size = .1,
                        segment.linetype = "dashed",
                        box.padding = .5,
                        segment.curvature = 0.2,
                        segment.ncp = 3,
                        segment.angle = 20)+
                coord_cartesian(xlim = c(min(log(glmnet_objs$engine$lambda)-2), 0))+
                theme(panel.grid.major = element_blank())+
                geom_hline(yintercept = 0,
                           linetype = 'dotted',
                           alpha = 0.5)
}

coef_plot = function(data) {
    
    data |>
    filter(term != '(Intercept)') |>
    ggplot(aes(x=estimate,
               y=reorder(term, estimate)))+
    geom_point()+
    geom_vline(xintercept = 0, linetype = 'dotted')+
    theme_minimal()+
    ylab("feature")
}